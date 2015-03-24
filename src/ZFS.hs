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
import qualified ZFS.Primitive.FS       as Z
import qualified ZFS.Primitive.NVPair   as Z

-- * Library initialization & finalization

initializeZFS = Z.libzfs_init
finalizeZFS = Z.libzfs_fini

-- | Get the underlying library handle from pool or filesystem handles
class GetHandle h where
  getHandle :: h -> IO Z.LibZFSHandle

instance GetHandle Z.ZPoolHandle where
  getHandle = Z.zpool_get_handle

instance GetHandle Z.ZFSHandle where
  getHandle = Z.zfs_get_handle

-- | Toggle error printing on or off.
printOnError = Z.libzfs_print_on_error

-- | Add message to zfs log
logHistory h str = C.useAsCString str $ Z.zpool_log_history h

errno = Z.libzfs_errno

errorAction = Z.libzfs_error_action >=> C.packCString

errorDescription = Z.libzfs_error_description >=> C.packCString

initializeMountTable = Z.libzfs_mnttab_init

finalizeMountTable = Z.libzfs_mnttab_fini

cacheMountTable = Z.libzfs_mnttab_cache

newtype MountTableEntry = MountTableEntry { fromMountTableEntry :: C.ByteString }

findMountTable h e t = C.useAsCString (fromMountTableEntry e) $ \str -> Z.libzfs_mnttab_find h str t

-- addMountToMountTable = Z.libzfsMnttabAdd

-- removeMountFromMountTable = Z.libzfsMnttabRemove

-- * Basic handle functions

newtype PoolName = PoolName { fromPoolName :: C.ByteString }
  deriving (Show, Eq, Ord)

data PoolOpenOptions = RefuseFaultedPools | AllowFaultedPools

-- | Open a handle to the given pool
openPool h (PoolName p) opt = do
  zh@(Z.ZPoolHandle h) <- C.useAsCString p $ \str -> case opt of
    RefuseFaultedPools -> Z.zpool_open h str
    AllowFaultedPools  -> Z.zpool_open_canfail h str
  return $! if h == nullPtr
    then Nothing
    else Just zh

closePool = Z.zpool_close

getPoolName = Z.zpool_get_name >=> (fmap PoolName . C.packCString)

getPoolState = Z.zpool_get_state

freeAllPoolHandles = Z.zpool_free_handles

iterateOverPools :: Z.LibZFSHandle -> (Z.ZPoolHandle -> a -> IO Int32) -> a -> IO Int32
iterateOverPools h f x = do
  sp <- S.newStablePtr x
  fp <- Z.wrapZPoolIterator $ \h ptr -> do
    val <- S.deRefStablePtr $ S.castPtrToStablePtr ptr
    r <- f h val
    return $ FC.CInt r
  (FC.CInt r) <- Z.zpool_iter h fp (S.castStablePtrToPtr sp)
  freeHaskellFunPtr fp
  S.freeStablePtr sp
  return r

-- * Functions to create and destroy pools

createPool h (PoolName p) nvRoot props fsprops = C.useAsCString p $ \str -> Z.zpool_create h str nvRoot props fsprops

-- | Destroy the given pool. It is up to the caller to ensure that there are no
-- datasets left in the pool.
destroyPool h log = C.useAsCString log $ Z.zpool_destroy h

-- * Functions to manipulate pool and vdev state

newtype Path = Path { fromPath :: C.ByteString }

-- | Scan the pool.
scanPool = Z.zpool_scan

-- | Clear the errors for the pool, or the particular device if specified.
clearPool h (Path p) rewind = C.useAsCString p $ \str -> Z.zpool_clear h str rewind

-- | Change the GUID for a pool.
reguidPool = Z.zpool_reguid

-- | Reopen the pool.
reopenPool = Z.zpool_reopen

-- | Bring the specified vdev online.
bringVirtualDeviceOnline :: Z.ZPoolHandle -> Path -> [Z.OnlineFlag] -> IO (Z.ZFSError, Z.VirtualDeviceState)
bringVirtualDeviceOnline h (Path p) fs = C.useAsCString p $ \str ->
  Z.zpool_vdev_online h str $ mask fs

data OfflineFlag = TemporarilyOffline | PermanentlyOffline
  deriving (Show, Eq)

-- | Take the specified vdev offline.
takeVirtualDeviceOffline h (Path p) b = C.useAsCString p $ \str -> Z.zpool_vdev_offline h str offlineMode
  where offlineMode = case b of
          TemporarilyOffline -> True
          PermanentlyOffline -> False

newtype NewDisk = NewDisk { fromNewDisk :: Path }
newtype OldDisk = OldDisk { fromOldDisk :: Path }
newtype Cookie = Cookie { fromCookie :: FC.CInt }

-- | Attach new disk (fully described by nvroot) to old disk.
-- If replacing is specified, the new disk will replace the old one.
attachVirtualDevice h (OldDisk (Path oldDisk)) (NewDisk (Path newDisk)) nvroot mc = C.useAsCString oldDisk $ \oldStr ->
  C.useAsCString newDisk $ \newStr -> Z.zpool_vdev_attach h oldStr newStr nvroot $ fromCookie $ fromMaybe (Cookie 0) mc

-- TODO splitVirtualDevice

newtype GUID = GUID { fromGUID :: FC.CULong }

-- | Mark the given vdev as faulted.
faultVirtualDevice h (GUID g) aux = Z.zpool_vdev_fault h g aux

-- | Mark the given vdev as degraded.
degradeVirtualDevice h (GUID g) aux = Z.zpool_vdev_degrade h g aux

-- | Similar to `clearPool`, but takes a GUID (needs better description once I understand this)
clearVirtualDevice h (GUID g) = Z.zpool_vdev_clear h g

data SpareFlag = SpareAvailable | SpareUnavailable
  deriving (Show, Eq)

data L2CacheFlag = L2CacheYes | L2CacheNo
  deriving (Show, Eq)

data LogFlag = LogYes | LogNo
  deriving (Show, Eq)

findVirtualDevice h (Path p) = C.useAsCString p $ \str -> do
  (nvl, sf, l2c, l) <- Z.zpool_find_vdev h str
  return ( nvl
         , if sf then SpareAvailable else SpareUnavailable
         , if l2c then L2CacheYes else L2CacheNo
         , if l then LogYes else LogNo
         )

findVirtualDeviceByPhysicalPath h (Path p) = C.useAsCString p $ \str -> do
  (nvl, sf, l2c, l) <- Z.zpool_find_vdev_by_physpath h str
  return ( nvl
         , if sf then SpareAvailable else SpareUnavailable
         , if l2c then L2CacheYes else L2CacheNo
         , if l then LogYes else LogNo
         )

-- | Wait timeout miliseconds for a newly created device to be available
-- from the given path.  There is a small window when a /dev/ device
-- will exist andd the udev link will not, so we must wait for the
-- symlink.  Depending on the udev rules this may take a few seconds.
waitForDiskLabel (Path p) t = C.useAsCString p $ \str -> Z.zpool_label_disk_wait str t

-- | Label an individual disk.  The name provided is the short name,
-- stripped of any leading /dev path.
labelDisk lh ph (Path p) = C.useAsCString p $ \str -> Z.zpool_label_disk lh ph str

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
-- getConfig :: ZPoolHandle -> Maybe NVList -> IO NVList

-- | Retrieves a list of enabled features and their refcounts and caches it in
--  the pool handle.
getFeatures = Z.zpool_get_features

type PoolIsMissing = Bool
-- | Refresh the vdev statistics associated with the given pool.  This is used in
-- iostat to show configuration changes and determine the delta from the last
-- time the function was called.  This function can fail, in case the pool has
-- been destroyed.
refreshStats :: Z.ZPoolHandle -> IO (Z.ZFSError, PoolIsMissing)
refreshStats = Z.zpool_refresh_stats

-- getErrorLog :: 

-- * Import and export functions

type SoftForce = Bool
-- | Exports the pool from the system. The caller must ensure that there are no
-- mounted datasets in the pool.
exportZPool :: Z.ZPoolHandle -> SoftForce -> C.ByteString -> IO Z.ZFSError
exportZPool h f msg = C.useAsCString msg $ Z.zpool_export h f

forceExportZPool h msg = C.useAsCString msg $ Z.zpool_export_force h

type Config = Z.NVList

-- | Applications should use importZPoolWithProperties to import a pool with
-- new properties value to be set.
importZPool :: Z.LibZFSHandle -> Config -> Maybe PoolName -> Maybe Path -> IO Z.ZFSError
importZPool h c mn mp = n $ \nstr -> p $ \pstr -> Z.zpool_import h c nstr pstr
  where
   n = maybe ($ nullPtr) C.useAsCString $ fmap fromPoolName mn
   p = maybe ($ nullPtr) C.useAsCString $ fmap fromPath mp

-- | Import the given pool using the known configuration and a list of
-- properties to be set. The configuration should have come from
-- importZPoolWithProperties. The 'newname' parameters control whether the pool
-- is imported with a different name.

-- importZPoolWithProperties :: LibZFSHandle -> NVList -> Maybe PoolTime -> NVList -> ????

printUnsupportedFeatures = Z.zpool_print_unsup_feat

-- * Search for pools to import
-- searchForZPools

-- * Miscellaneous pool functions

-- * Basic handle manipulations. These functions do not create or destroy the
-- underlying datasets, only the references to them.

-- |  Opens the given snapshot, filesystem, or volume.   The 'types'
-- argument is a mask of acceptable types.  The function will print an
-- appropriate error message and return Nothing if it can't be opened.
openZFSHandle :: Z.LibZFSHandle -> Path -> [Z.ZFSType] -> IO (Maybe Z.ZFSHandle)
openZFSHandle h (Path p) ts = do
  zh@(Z.ZFSHandle h') <- C.useAsCString p $ \str -> Z.zfs_open h str $ mask ts
  return $! if h' == nullPtr
    then Nothing
    else Just zh

duplicateZFSHandle = Z.zfs_handle_dup

closeZFSHandle = Z.zfs_close

getZFSHandleType = error "TODO"

getName = Z.zfs_get_name >=> C.packCString

getPoolHandle = Z.zfs_get_pool_handle

-- * Property management functions. Some functions are shared with the kernel,
-- and are found in sys/fs/zfs.h

-- ** zfs dataset property management

-- ** zpool property management

-- ** Functions shared by zfs and zpool property management

-- * Functions to create and destroy datasets

-- * Miscellaneous functions

zfsTypeToName = Z.zfs_type_to_name >=> C.packCString

refreshProperties = Z.zfs_refresh_properties

nameValid str t = C.useAsCString str $ \p -> Z.zfs_name_valid p t

-- | Given a name, determine whether or not it's a valid path
-- (starts with '/' or "./").  If so, walk the mnttab trying
-- to match the device number.  If not, treat the path as an
-- fs/vol/snap name.
pathToZFSHandle h (Path p) t = do
  zh@(Z.ZFSHandle inner) <- C.useAsCString p $ \str -> Z.zfs_path_to_zhandle h str t
  return $! if inner == nullPtr
    then Nothing
    else Just zh

-- | Finds whether the dataset of the given type(s) exists.
datasetExists h (Path p) t = do
  -- TODO support type unions?
  C.useAsCString p $ \str -> Z.zfs_dataset_exists h str t

spaVersion = Z.zfs_spa_version

-- | Append partition suffix to an otherwise fully qualified device path.
-- This is used to generate the name the full path as its stored in
-- ZPOOL_CONFIG_PATH for whole disk devices.  On success the new length
-- of 'path' will be returned on error a negative value is returned.
appendPartition (Path p) = do
  -- make room in final copy for appending bits
  C.useAsCString (p <> "\0\0\0\0\0\0") $ \str -> do
    len <- Z.zfs_append_partition str $ fromIntegral (C.length p + 6)
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
  (mounted, where_) <- C.useAsCString str $ Z.is_mounted h
  where_' <- if mounted
    then fmap (Just . Path) $ C.packCString where_
    else return Nothing
  return (mounted, where_')

zfsIsMounted h = do
  (mounted, where_) <- Z.zfs_is_mounted h
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
  mountPtrFun $ \str -> Z.zfs_unmount h str flags

-- TODO flags here too.
unmountAll h flags = Z.zfs_unmountall h flags

-- * Share support

isShared = Z.zfs_is_shared

share = Z.zfs_share

unshare = Z.zfs_unshare

-- ** Protocol-specific share support

isSharedNFS h = do
  (shared, p) <- Z.zfs_is_shared_nfs h
  p' <- C.packCString p
  return (shared, p)

isSharedSMB h = do
  (shared, p) <- Z.zfs_is_shared_smb h
  p' <- C.packCString p
  return (shared, p)

shareNFS = Z.zfs_share_nfs
shareSMB = Z.zfs_share_smb

unshareNFS = Z.zfs_unshare_nfs
unshareSMB = Z.zfs_unshare_smb

unshareAllNFS = Z.zfs_unshareall_nfs
unshareAllSMB = Z.zfs_unshareall_smb

unshareAllByPath h path = do
  let pathPtrFun f = case path of
                       Nothing -> f nullPtr
                       Just (Path p) -> C.useAsCString p f
  pathPtrFun $ \str -> Z.zfs_unshareall_bypath h str

-- * Utility functions
niceNumber = error "TODO"
niceStringToNumber = error "TODO"

isPoolInUse h (Fd fd) = do
  (err, st, n, inUse) <- Z.zpool_in_use h fd
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
