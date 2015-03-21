{-|
Module      : ZFS
Description : ZFS API bindings
Copyright   : (c) Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental
Portability : Only tested on Linux x86_64

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE OverloadedStrings #-}
module ZFS where

import           Control.Monad
import           Data.Bits              ((.|.))
import qualified Data.ByteString.Char8  as C
import qualified Data.List              as L
import           Data.Monoid            ((<>))
import           Data.Maybe             (fromMaybe)
import           Data.Int
import qualified Data.Text              as T
import qualified Data.Text.Foreign      as T
import           Data.Word
import qualified Foreign.C              as FC
import           Foreign.Ptr            (nullPtr, freeHaskellFunPtr)
import qualified Foreign.StablePtr      as S
import           System.Posix.Types     (Fd(..))
import qualified ZFS.Primitive          as Z
import qualified ZFS.Sys.FileSystem.ZFS as Z
import qualified ZFS.Sys.NVPair         as Z

-- * Library initialization & finalization

initializeZFS = Z.libzfsInit
finalizeZFS = Z.libzfsFini

-- | Get the underlying library handle from pool or filesystem handles
class GetHandle h where
  getHandle :: h -> IO Z.LibZfsHandle

instance GetHandle Z.ZpoolHandle where
  getHandle = Z.zpoolGetHandle

instance GetHandle Z.ZfsHandle where
  getHandle = Z.zfsGetHandle

-- | Toggle error printing on or off.
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

-- * Basic handle functions

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

-- * Functions to create and destroy pools

createPool h (PoolName p) nvRoot props fsprops = C.useAsCString p $ \str -> Z.zpoolCreate h str nvRoot props fsprops

-- | Destroy the given pool. It is up to the caller to ensure that there are no
-- datasets left in the pool.
destroyPool h log = C.useAsCString log $ Z.zpoolDestroy h

-- * Functions to manipulate pool and vdev state

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
  Z.zpoolVdevOnline h str $ mask fs

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

-- * Functions to manage pool properties

-- zpool_get_prop
-- zpool_get_prop_literal
-- zpool_get_prop_int
-- zpool_prop_to_name
-- zpool_prop_values

-- * Pool health statistics

-- zpool_get_status
-- zpool_import_status
-- zpool_dump_ddt

-- * Statistics and configuration functions

-- | Retrieve the configuration for the given pool. The configuration is a nvlist
-- describing the vdevs, as well as the statistics associated with each one.
-- getConfig :: ZpoolHandle -> Maybe NVList -> IO NVList

-- | Retrieves a list of enabled features and their refcounts and caches it in
--  the pool handle.
getFeatures = Z.zpoolGetFeatures

type PoolIsMissing = Bool
-- | Refresh the vdev statistics associated with the given pool.  This is used in
-- iostat to show configuration changes and determine the delta from the last
-- time the function was called.  This function can fail, in case the pool has
-- been destroyed.
refreshStats :: Z.ZpoolHandle -> IO (Z.ZfsError, PoolIsMissing)
refreshStats = Z.zpoolRefreshStats

-- getErrorLog :: 

-- * Import and export functions

type SoftForce = Bool
-- | Exports the pool from the system. The caller must ensure that there are no
-- mounted datasets in the pool.
exportZPool :: Z.ZpoolHandle -> SoftForce -> C.ByteString -> IO Z.ZfsError
exportZPool h f msg = C.useAsCString msg $ Z.zpoolExport h f

forceExportZPool h msg = C.useAsCString msg $ Z.zpoolExportForce h

type Config = Z.NVList

-- | Applications should use importZPoolWithProperties to import a pool with
-- new properties value to be set.
importZPool :: Z.LibZfsHandle -> Config -> Maybe PoolName -> Maybe Path -> IO Z.ZfsError
importZPool h c mn mp = n $ \nstr -> p $ \pstr -> Z.zpoolImport h c nstr pstr
  where
   n = maybe ($ nullPtr) C.useAsCString $ fmap fromPoolName mn
   p = maybe ($ nullPtr) C.useAsCString $ fmap fromPath mp

-- | Import the given pool using the known configuration and a list of
-- properties to be set. The configuration should have come from
-- importZPoolWithProperties. The 'newname' parameters control whether the pool
-- is imported with a different name.

-- importZPoolWithProperties :: LibZfsHandle -> NVList -> Maybe PoolTime -> NVList -> ????

printUnsupportedFeatures = Z.zpoolPrintUnsupFeat

-- * Search for pools to import
-- searchForZPools

-- * Miscellaneous pool functions

-- * Basic handle manipulations. These functions do not create or destroy the
-- underlying datasets, only the references to them.

-- |  Opens the given snapshot, filesystem, or volume.   The 'types'
-- argument is a mask of acceptable types.  The function will print an
-- appropriate error message and return Nothing if it can't be opened.
openZFSHandle :: Z.LibZfsHandle -> Path -> [Z.ZfsType] -> IO (Maybe Z.ZfsHandle)
openZFSHandle h (Path p) ts = do
  zh@(Z.ZfsHandle h') <- C.useAsCString p $ \str -> Z.zfsOpen h str $ mask ts
  return $! if h' == nullPtr
    then Nothing
    else Just zh

duplicateZFSHandle = Z.zfsHandleDup

closeZFSHandle = Z.zfsClose

getZFSHandleType = error "TODO"

getName = Z.zfsGetName >=> C.packCString

getPoolHandle = Z.zfsGetPoolHandle

-- * Property management functions. Some functions are shared with the kernel,
-- and are found in sys/fs/zfs.h

-- ** zfs dataset property management

-- ** zpool property management

-- ** Functions shared by zfs and zpool property management

-- * Functions to create and destroy datasets

-- * Miscellaneous functions

zfsTypeToName = Z.zfsTypeToName >=> C.packCString

refreshProperties = Z.zfsRefreshProperties

nameValid str t = C.useAsCString str $ \p -> Z.zfsNameValid p t

-- | Given a name, determine whether or not it's a valid path
-- (starts with '/' or "./").  If so, walk the mnttab trying
-- to match the device number.  If not, treat the path as an
-- fs/vol/snap name.
pathToZFSHandle h (Path p) t = do
  zh@(Z.ZfsHandle inner) <- C.useAsCString p $ \str -> Z.zfsPathToZhandle h str t
  return $! if inner == nullPtr
    then Nothing
    else Just zh

-- | Finds whether the dataset of the given type(s) exists.
datasetExists h (Path p) t = do
  -- TODO support type unions?
  C.useAsCString p $ \str -> Z.zfsDatasetExists h str t

spaVersion = Z.zfsSpaVersion

-- | Append partition suffix to an otherwise fully qualified device path.
-- This is used to generate the name the full path as its stored in
-- ZPOOL_CONFIG_PATH for whole disk devices.  On success the new length
-- of 'path' will be returned on error a negative value is returned.
appendPartition (Path p) = do
  -- make room in final copy for appending bits
  C.useAsCString (p <> "\0\0\0\0\0\0") $ \str -> do
    len <- Z.zfsAppendPartition str $ fromIntegral (C.length p + 6)
    let l = fromIntegral l
    if l < 0
      then return Nothing
      else fmap Just $ C.packCStringLen (str, fromIntegral len)

-- | Given a shorthand device name check if a file by that name exists in any
-- of the 'zpool_default_import_path' or ZPOOL_IMPORT_PATH directories.  If
-- one is found, store its fully qualified path in the 'path' buffer passed
resolveShortname = error "TODO"

-- | Given either a shorthand or fully qualified path name look for a match
-- against 'cmp'.  The passed name will be expanded as needed for comparison
-- purposes and redundant slashes stripped to ensure an accurate match.
comparePathnames = error "TODO"

-- * Mount support functions

-- | Checks to see if the mount is active. If the filesystem is mounted,
-- returns true and the current mountpoint
isMounted h str = do
  -- TODO what is the str argument in isMounted supposed to mean?
  (mounted, where_) <- C.useAsCString str $ Z.isMounted h
  where_' <- if mounted
    then fmap (Just . Path) $ C.packCString where_
    else return Nothing
  return (mounted, where_')

zfsIsMounted h = do
  (mounted, where_) <- Z.zfsIsMounted h
  where_' <- if mounted
    then fmap (Just . Path) $ C.packCString where_
    else return Nothing
  return (mounted, where_')

-- | Unmount the given filesystem.
unmount h mount flags = do
  -- TODO what is flags here?
  let mountPtrFun f = case mount of
                        Nothing -> f nullPtr
                        Just (Path p) -> C.useAsCString p f
  mountPtrFun $ \str -> Z.zfsUnmount h str flags

-- TODO flags here too.
unmountAll h flags = Z.zfsUnmountall h flags

-- * Share support

isShared = Z.zfsIsShared

share = Z.zfsShare

unshare = Z.zfsUnshare

-- ** Protocol-specific share support

isSharedNFS h = do
  (shared, p) <- Z.zfsIsSharedNfs h
  p' <- C.packCString p
  return (shared, p)

isSharedSMB h = do
  (shared, p) <- Z.zfsIsSharedNfs h
  p' <- C.packCString p
  return (shared, p)

shareNFS = Z.zfsShareNfs
shareSMB = Z.zfsShareSmb

unshareNFS = Z.zfsUnshareNfs
unshareSMB = Z.zfsUnshareSmb

unshareAllNFS = Z.zfsUnshareallNfs
unshareAllSMB = Z.zfsUnshareallSmb

unshareAllByPath h path = do
  let pathPtrFun f = case path of
                       Nothing -> f nullPtr
                       Just (Path p) -> C.useAsCString p f
  pathPtrFun $ \str -> Z.zfsUnshareallBypath h str

-- * Utility functions
niceNumber = error "TODO"
niceStringToNumber = error "TODO"

isPoolInUse h (Fd fd) = do
  (err, st, n, inUse) <- Z.zpoolInUse h fd
  let state = if inUse
                then Just . toEnum . fromIntegral $ st
                else Nothing
  name <- if inUse
    then fmap (Just . PoolName) $ C.packCString n
    else return Nothing
  return (err, state, name, inUse)

-- ** Label manipulation
-- ** Management interfaces for SMB ACL

-- Haskell utilities
mask :: (Enum a, Num b) => [a] -> b
mask = fromIntegral . L.foldl' (\i f -> i .|. fromEnum f) 0
