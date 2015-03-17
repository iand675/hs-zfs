module ZFS where

import           Control.Monad
import           Data.Bits              ((.|.))
import qualified Data.ByteString.Char8  as C
import qualified Data.List              as L
import           Data.Maybe             (fromMaybe)
import           Data.Int
import qualified Data.Text              as T
import qualified Data.Text.Foreign      as T
import           Data.Word
import qualified Foreign.C              as FC
import           Foreign.Ptr            (nullPtr, freeHaskellFunPtr)
import qualified Foreign.StablePtr      as S
import qualified ZFS.Primitive          as Z
import qualified ZFS.Sys.FileSystem.ZFS as Z

initializeZFS = Z.libzfsInit

finalizeZFS = Z.libzfsFini

class GetHandle h where
  getHandle :: h -> IO Z.LibZfsHandle

instance GetHandle Z.ZpoolHandle where
  getHandle = Z.zpoolGetHandle

instance GetHandle Z.ZfsHandle where
  getHandle = Z.zfsGetHandle

printOnError = Z.libzfsPrintOnError

-- | Add message to zfs log
logHistory h str = C.useAsCString str $ Z.zpoolLogHistory h

errno = Z.libzfsErrno

errorAction = Z.libzfsErrorAction >=> C.packCString

errorDescription = Z.libzfsErrorDescription >=> C.packCString

initializeMountTable = Z.libzfsMnttabInit

finalizeMountTable = Z.libzfsMnttabFini

cacheMountTable = Z.libzfsMnttabCache

newtype MountTableEntry = MountTableEntry { fromMountTableEntry :: C.ByteString }

findMountTable h e t = C.useAsCString (fromMountTableEntry e) $ \str -> Z.libzfsMnttabFind h str t

-- addMountToMountTable = Z.libzfsMnttabAdd

-- removeMountFromMountTable = Z.libzfsMnttabRemove

newtype PoolName = PoolName { fromPoolName :: C.ByteString }

data PoolOpenOptions = RefuseFaultedPools | AllowFaultedPools

-- | Open a handle to the given pool
openPool h (PoolName p) opt = do
  zh@(Z.ZpoolHandle h) <- C.useAsCString p $ \str -> case opt of
    RefuseFaultedPools -> Z.zpoolOpen h str
    AllowFaultedPools  -> Z.zpoolOpenCanfail h str
  return $! if h == nullPtr
    then Nothing
    else Just zh

closePool = Z.zpoolClose

getPoolName = Z.zpoolGetName >=> (fmap PoolName . C.packCString)

getPoolState = Z.zpoolGetState

freeAllPoolHandles = Z.zpoolFreeHandles

iterateOverPools :: Z.LibZfsHandle -> (Z.ZpoolHandle -> a -> IO Int32) -> a -> IO Int32
iterateOverPools h f x = do
  sp <- S.newStablePtr x
  fp <- Z.wrapZPoolIterator $ \h ptr -> do
    val <- S.deRefStablePtr $ S.castPtrToStablePtr ptr
    r <- f h val
    return $ FC.CInt r
  (FC.CInt r) <- Z.zpoolIter h fp (S.castStablePtrToPtr sp)
  freeHaskellFunPtr fp
  S.freeStablePtr sp
  return r

createPool h (PoolName p) nvRoot props fsprops = C.useAsCString p $ \str -> Z.zpoolCreate h str nvRoot props fsprops

newtype Path = Path { fromPath :: C.ByteString }

-- | Scan the pool.
scanPool = Z.zpoolScan

-- | Clear the errors for the pool, or the particular device if specified.
clearPool h (Path p) rewind = C.useAsCString p $ \str -> Z.zpoolClear h str rewind

-- | Change the GUID for a pool.
reguidPool = Z.zpoolReguid

-- | Reopen the pool.
reopenPool = Z.zpoolReopen

-- | Bring the specified vdev online.
bringVirtualDeviceOnline :: Z.ZpoolHandle -> Path -> [Z.OnlineFlag] -> IO (Z.ZfsError, Z.VirtualDeviceState)
bringVirtualDeviceOnline h (Path p) fs = C.useAsCString p $ \str ->
  Z.zpoolVdevOnline h str $ fromIntegral $ (L.foldl' (\i f -> i .|. fromEnum f) 0 fs)

data OfflineFlag = TemporarilyOffline | PermanentlyOffline
  deriving (Show, Eq)

-- | Take the specified vdev offline.
takeVirtualDeviceOffline h (Path p) b = C.useAsCString p $ \str -> Z.zpoolVdevOffline h str offlineMode
  where offlineMode = case b of
          TemporarilyOffline -> True
          PermanentlyOffline -> False

newtype NewDisk = NewDisk { fromNewDisk :: Path }
newtype OldDisk = OldDisk { fromOldDisk :: Path }
newtype Cookie = Cookie { fromCookie :: FC.CInt }

-- | Attach new disk (fully described by nvroot) to old disk.
-- If replacing is specified, the new disk will replace the old one.
attachVirtualDevice h (OldDisk (Path oldDisk)) (NewDisk (Path newDisk)) nvroot mc = C.useAsCString oldDisk $ \oldStr ->
  C.useAsCString newDisk $ \newStr -> Z.zpoolVdevAttach h oldStr newStr nvroot $ fromCookie $ fromMaybe (Cookie 0) mc

-- TODO splitVirtualDevice

newtype GUID = GUID { fromGUID :: FC.CULong }

-- | Mark the given vdev as faulted.
faultVirtualDevice h (GUID g) aux = Z.zpoolVdevFault h g aux

-- | Mark the given vdev as degraded.
degradeVirtualDevice h (GUID g) aux = Z.zpoolVdevDegrade h g aux

-- | Similar to `clearPool`, but takes a GUID (needs better description once I understand this)
clearVirtualDevice h (GUID g) = Z.zpoolVdevClear h g

data SpareFlag = SpareAvailable | SpareUnavailable
  deriving (Show, Eq)

data L2CacheFlag = L2CacheYes | L2CacheNo
  deriving (Show, Eq)

data LogFlag = LogYes | LogNo
  deriving (Show, Eq)

findVirtualDevice h (Path p) = C.useAsCString p $ \str -> do
  (nvl, sf, l2c, l) <- Z.zpoolFindVdev h str
  return ( nvl
         , if sf then SpareAvailable else SpareUnavailable
         , if l2c then L2CacheYes else L2CacheNo
         , if l then LogYes else LogNo
         )

findVirtualDeviceByPhysicalPath h (Path p) = C.useAsCString p $ \str -> do
  (nvl, sf, l2c, l) <- Z.zpoolFindVdevByPhyspath h str
  return ( nvl
         , if sf then SpareAvailable else SpareUnavailable
         , if l2c then L2CacheYes else L2CacheNo
         , if l then LogYes else LogNo
         )

-- | Wait timeout miliseconds for a newly created device to be available
-- from the given path.  There is a small window when a /dev/ device
-- will exist andd the udev link will not, so we must wait for the
-- symlink.  Depending on the udev rules this may take a few seconds.
waitForDiskLabel (Path p) t = C.useAsCString p $ \str -> Z.zpoolLabelDiskWait str t

-- | Label an individual disk.  The name provided is the short name,
-- stripped of any leading /dev path.
labelDisk lh ph (Path p) = C.useAsCString p $ \str -> Z.zpoolLabelDisk lh ph str

-- Functions to manage pool properties


