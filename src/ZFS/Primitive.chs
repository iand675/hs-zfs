module ZFS.Primitive where
import Data.Word
import Foreign
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Marshal.Utils
{#import ZFS.Primitive.AVL #}
{#import ZFS.Primitive.FS #}
{#import ZFS.Primitive.NVPair #} hiding (peekBool)

#define HAVE_IOCTL_IN_SYS_IOCTL_H
#include <libzfs.h>

{#context lib = "zfs" #}

-- Miscellaneous ZFS constants
zfs_maxnamelen = {#const ZFS_MAXNAMELEN #}
zpool_maxnamelen = {#const ZPOOL_MAXNAMELEN #}
zfs_maxproplen = {#const ZFS_MAXPROPLEN #}
zpool_maxproplen = {#const ZPOOL_MAXPROPLEN #}

-- Default device paths
disk_root = {#const DISK_ROOT #}
udisk_root = {#const UDISK_ROOT #}

-- Default wait time for a device name to be created

disk_label_wait = {#const DISK_LABEL_WAIT #}

default_import_path_size = {#const DEFAULT_IMPORT_PATH_SIZE #}

-- zpool_default_import_path ::

-- libzfs errors
{#enum zfs_error as ZFSError {underscoreToCase} deriving (Show, Eq)#}

{#pointer *zfs_perm_node as ZFSPermNode newtype #}
{#pointer *zfs_allow_node as ZFSAllowNode newtype #}
{#pointer *zfs_allow as ZFSAllow newtype #}

{#pointer *zfs_handle as ZFSHandle newtype #}
{#pointer *zpool_handle as ZPoolHandle newtype #}
{#pointer *libzfs_handle as LibZFSHandle newtype #}

toZFSHandle = ZFSHandle . castPtr
toZPoolHandle = ZPoolHandle . castPtr
toLibZFSHandle = LibZFSHandle . castPtr

fromZFSHandle (ZFSHandle p) = castPtr p
fromZPoolHandle (ZPoolHandle p) = castPtr p
fromLibZFSHandle (LibZFSHandle p) = castPtr p

makeEnum :: (Integral a, Enum b) => a -> b
makeEnum = toEnum . fromIntegral

peekEnum :: (Enum a) => Ptr CInt -> IO a
peekEnum = fmap makeEnum . peek

peekBool :: Ptr CInt -> IO Bool
peekBool = fmap toBool . peek

-- Library initialization
{#fun libzfs_init
  {} -> `LibZFSHandle' toLibZFSHandle #}

{#fun libzfs_fini
  {fromLibZFSHandle `LibZFSHandle'} -> `()' #}

{#fun zpool_get_handle
  {fromZPoolHandle `ZPoolHandle'} -> `LibZFSHandle' toLibZFSHandle #}

{#fun zfs_get_handle
  {fromZFSHandle `ZFSHandle'} -> `LibZFSHandle' toLibZFSHandle #}

{#fun libzfs_print_on_error
  {fromLibZFSHandle `LibZFSHandle',
   `Bool'} -> `()' #}

-- | Concatenates all arguments into output string. Args: arg count, args, output string, output string length.
{#fun zfs_save_arguments
  {`Int',
   id `Ptr CString',
   `CString',
   `Int'} -> `()' #}

-- | Add message to zpool log
{#fun zpool_log_history
  { fromLibZFSHandle `LibZFSHandle'
  , id `CString' } -> `ZFSError' makeEnum #}

{#fun libzfs_errno
  { fromLibZFSHandle `LibZFSHandle' } -> `ZFSError' makeEnum #}

{#fun libzfs_error_action
  { fromLibZFSHandle `LibZFSHandle' } -> `CString' #}

{#fun libzfs_error_description
  { fromLibZFSHandle `LibZFSHandle' } -> `CString' #}

{#fun libzfs_mnttab_init
  { fromLibZFSHandle `LibZFSHandle' } -> `()' #}

{#fun libzfs_mnttab_fini
  { fromLibZFSHandle `LibZFSHandle' } -> `()' #}

{#fun libzfs_mnttab_cache
  { fromLibZFSHandle `LibZFSHandle',
    `Bool' } -> `()' #}

{#pointer *mnttab as MountTable newtype #}

{#fun libzfs_mnttab_find
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `MountTable' } -> `ZFSError' makeEnum #}

{#fun libzfs_mnttab_add
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `CString',
    `CString' } -> `()' #}

{#fun libzfs_mnttab_remove
  { fromLibZFSHandle `LibZFSHandle',
    `CString' } -> `()' #}

-- Basic handle functions

{#fun zpool_open
  { fromLibZFSHandle `LibZFSHandle',
    `CString' } -> `ZPoolHandle' toZPoolHandle #}

{#fun zpool_open_canfail
  { fromLibZFSHandle `LibZFSHandle',
    `CString' } -> `ZPoolHandle' toZPoolHandle #}

{#fun zpool_close
  { fromZPoolHandle `ZPoolHandle' } -> `()' #}

{#fun zpool_get_name
  { fromZPoolHandle `ZPoolHandle' } -> `CString' #}

{#fun zpool_get_state
  { fromZPoolHandle `ZPoolHandle' } -> `PoolState' makeEnum #}

-- {#fun zpool_state_to_name
-- zpool_pool_state_to_name

-- Iterate over all active pools in the system
{#fun zpool_free_handles
  { fromLibZFSHandle `LibZFSHandle' } -> `()' #}

type ZPoolIterator a = ZPoolHandle -> Ptr a -> IO CInt

foreign import ccall "wrapper"
  wrapZPoolIterator :: ZPoolIterator a -> IO (FunPtr (ZPoolIterator a))

{#fun zpool_iter
  { fromLibZFSHandle `LibZFSHandle',
    castFunPtr `FunPtr (ZPoolIterator a)',
    castPtr `Ptr a' } -> `CInt' #}

-- Functions to create and destroy pools
{#fun zpool_create
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `NVList',
    `NVList',
    `NVList' } -> `ZFSError' makeEnum #}

{#fun zpool_destroy
  { fromZPoolHandle `ZPoolHandle',
    `CString' } -> `ZFSError' makeEnum #}

{#pointer *splitflags_t as SplitFlags newtype #}

-- Functions to manipulate pool and vdev state
{#fun zpool_scan
  { fromZPoolHandle `ZPoolHandle',
    `PoolScanFunction' } -> `ZFSError' makeEnum #}

{#fun zpool_clear
  { fromZPoolHandle `ZPoolHandle',
    `CString',
    `NVList' } -> `ZFSError' makeEnum #}

{#fun zpool_reguid
  { fromZPoolHandle `ZPoolHandle' } -> `ZFSError' makeEnum #}

{#fun zpool_reopen
  { fromZPoolHandle `ZPoolHandle' } -> `ZFSError' makeEnum #}

{#fun zpool_vdev_online
  { fromZPoolHandle `ZPoolHandle',
    `CString',
    `Int',
    alloca- `VirtualDeviceState' peekEnum*} -> `ZFSError' makeEnum #}

{#fun zpool_vdev_offline
  { fromZPoolHandle `ZPoolHandle',
    `CString',
    `Bool' } -> `ZFSError' makeEnum #}

{#fun zpool_vdev_attach
  { fromZPoolHandle `ZPoolHandle',
    `CString',
    `CString',
    `NVList',
    `CInt' } -> `ZFSError' makeEnum #}

{#fun zpool_vdev_detach
  { fromZPoolHandle `ZPoolHandle',
    `CString' } -> `ZFSError' makeEnum #}

{#fun zpool_vdev_remove
  { fromZPoolHandle `ZPoolHandle',
    `CString' } -> `ZFSError' makeEnum #}

{-
TODO: Needs a wrapper function because split_flags_t is a bare struct
{#fun zpool_vdev_split
  { fromZPoolHandle `ZPoolHandle',
    `CString',
    `Ptr NVList',
    `NVList',
    % `SplitFlags' } -> `ZFSError' makeEnum #}
-}

{#fun zpool_vdev_fault
  { fromZPoolHandle `ZPoolHandle',
    `CULong',
    `VirtualDeviceAuxilliaryState' } -> `ZFSError' makeEnum #}

{#fun zpool_vdev_degrade
  { fromZPoolHandle `ZPoolHandle',
    `CULong',
    `VirtualDeviceAuxilliaryState' } -> `ZFSError' makeEnum #}

{#fun zpool_vdev_clear
  { fromZPoolHandle `ZPoolHandle',
    `CULong' } -> `ZFSError' makeEnum #}

{#fun zpool_find_vdev
  { fromZPoolHandle `ZPoolHandle',
    `CString',
    alloca- `Bool' peekBool*,
    alloca- `Bool' peekBool*,
    alloca- `Bool' peekBool*} -> `NVList' #}

{#fun zpool_find_vdev_by_physpath
  { fromZPoolHandle `ZPoolHandle',
    `CString',
    alloca- `Bool' peekBool*,
    alloca- `Bool' peekBool*,
    alloca- `Bool' peekBool*} -> `NVList' #}

{#fun zpool_label_disk_wait
  { `CString',
    `Int' } -> `ZFSError' makeEnum #}

{#fun zpool_label_disk
  { fromLibZFSHandle `LibZFSHandle',
    fromZPoolHandle `ZPoolHandle',
    `CString' } -> `ZFSError' makeEnum #}

-- Functions to manage pool properties

{#fun zpool_set_prop
  { fromZPoolHandle `ZPoolHandle',
    `CString',
    `CString' } -> `ZFSError' makeEnum #}

-- zpool_get_prop
-- zpool_get_prop_literal
-- zpool_get_prop_int
-- zpool_prop_to_name
-- zpool_prop_values

-- Pool health statistics
{#enum zpool_status_t as ZPoolStatus {underscoreToCase} deriving (Show, Eq)#}

-- zpool_get_status
-- zpool_import_status
-- zpool_dump_ddt

-- Statistics and configuration functions

-- {#enum zpool_status_t as ZPoolStatus {underscoreToCase} deriving (Show, Eq)#}

{#fun zpool_get_config
  { fromZPoolHandle `ZPoolHandle',
    id `Ptr NVList' } -> `NVList' #}

{#fun zpool_get_features
  { fromZPoolHandle `ZPoolHandle' } -> `NVList' #}

{#fun zpool_refresh_stats
  { fromZPoolHandle `ZPoolHandle',
    alloca- `Bool' peekBool* } -> `ZFSError' makeEnum #}

{#fun zpool_get_errlog
  { fromZPoolHandle `ZPoolHandle',
    id `Ptr NVList' } -> `ZFSError' makeEnum #}

-- Import and export functions

{#fun zpool_export
  { fromZPoolHandle `ZPoolHandle',
    `Bool',
    `CString' } -> `ZFSError' makeEnum #}

{#fun zpool_export_force
  { fromZPoolHandle `ZPoolHandle',
    `CString' } -> `ZFSError' makeEnum #}

{#fun zpool_import
  { fromLibZFSHandle `LibZFSHandle',
    `NVList',
    `CString',
    `CString' } -> `ZFSError' makeEnum #}

{#fun zpool_import_props
  { fromLibZFSHandle `LibZFSHandle',
    `NVList',
    `CString',
    `NVList',
    `Int' } -> `ZFSError' makeEnum #}

{#fun zpool_print_unsup_feat
  { `NVList' } -> `()' #}

-- Search for pools to import

{#pointer *importargs_t as ImportArgs newtype #}

{#fun zpool_search_import
  { fromLibZFSHandle `LibZFSHandle',
    `ImportArgs' } -> `NVList' #}

-- Legacy pool search routines would go here, but skipping them for now.

-- Miscellaneous pool functions
{#pointer *zfs_cmd as ZFSCommand newtype #}

toZFSCommand = ZFSCommand . castPtr
fromZFSCommand (ZFSCommand p) = castPtr p

-- zfs_history_event_names
{#fun zpool_vdev_name
  { fromLibZFSHandle `LibZFSHandle',
    fromZPoolHandle `ZPoolHandle',
    `NVList',
    `Bool' } -> `CString' #}

{#fun zpool_upgrade
  { fromZPoolHandle `ZPoolHandle',
    `Word64' } -> `ZFSError' makeEnum #}

{#fun zpool_get_history
  { fromZPoolHandle `ZPoolHandle',
    id `Ptr NVList' } -> `ZFSError' makeEnum #}

{#fun zpool_history_unpack
  { `CString',
    `CULong',
    alloca- `CULong' peek*,
    id `Ptr (Ptr NVList)',
    alloca- `CUInt' peek*} -> `ZFSError' makeEnum #}

{#fun zpool_events_next
  { fromLibZFSHandle `LibZFSHandle',
    id `Ptr NVList',
    alloca- `CInt' peek*,
    `CUInt',
    `CInt' } -> `ZFSError' makeEnum #}

{#fun zpool_events_clear
  { fromLibZFSHandle `LibZFSHandle',
    alloca- `CInt' peek* } -> `ZFSError' makeEnum #}

{#fun zpool_events_seek
  { fromLibZFSHandle `LibZFSHandle',
    `CULong',
    `CInt' } -> `ZFSError' makeEnum #}

{#fun zpool_obj_to_path
  { fromZPoolHandle `ZPoolHandle',
    `CULong',
    `CULong',
    `CString',
    `CULong' } -> `()' #}

{#fun zfs_ioctl
  { fromLibZFSHandle `LibZFSHandle',
    `CInt',
    `ZFSCommand' } -> `ZFSError' makeEnum #}

{#fun zpool_get_physpath
  { fromZPoolHandle `ZPoolHandle',
    `CString',
    `CULong' } -> `ZFSError' makeEnum #}

{#fun zpool_explain_recover
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `CInt',
    `NVList' } -> `()' #}

-- Basic handle manipulations. These functions do not create or destroy the
-- underlying datasets, only the references to them.
{#fun zfs_open
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `CInt' } -> `ZFSHandle' toZFSHandle #}

{#fun zfs_handle_dup
  { fromZFSHandle `ZFSHandle' } -> `ZFSHandle' toZFSHandle #}

{#fun zfs_close
  { fromZFSHandle `ZFSHandle' } -> `()' #}

-- {#fun zfs_get_type
{#fun zfs_get_name
  { fromZFSHandle `ZFSHandle' } -> `CString' #}

{#fun zfs_get_pool_handle
  { fromZFSHandle `ZFSHandle' } -> `ZPoolHandle' toZPoolHandle #}

-- Property management functions. Some functions are shared with the kernel,
-- and are found in sys/fs/zfs.h

-- zfs dataset property management
{#fun zfs_prop_default_string
  { `ZFSProp' } -> `CString' #}

{#fun zfs_prop_default_numeric
  { `ZFSProp' } -> `CULong' #}

{#fun zfs_prop_column_name
  { `ZFSProp' } -> `CString' #}

{#fun zfs_prop_align_right
  { `ZFSProp' } -> `Bool' #}

{#fun zfs_valid_proplist
  { fromLibZFSHandle `LibZFSHandle',
    `ZFSType',
    `NVList',
    `CULong',
    fromZFSHandle `ZFSHandle',
    `CString' } -> `NVList' #}

{#fun zfs_prop_to_name
  { `ZFSProp' } -> `CString' #}

{#fun zfs_prop_set
  { fromZFSHandle `ZFSHandle',
    `CString',
    `CString' } -> `ZFSError' makeEnum #}

{#fun zfs_prop_get
  { fromZFSHandle `ZFSHandle',
    `ZFSProp',
    `CString',
    `CULong',
    alloca- `ZPropSource',
    `CString',
    `CULong',
    `Bool'} -> `ZFSError' makeEnum #}

{#fun zfs_prop_get_recvd
  { fromZFSHandle `ZFSHandle',
    `CString',
    `CString',
    `CULong',
    `Bool' } -> `ZFSError' makeEnum #}

{#fun zfs_prop_get_numeric
  { fromZFSHandle `ZFSHandle',
    `ZPropSource',
    alloca- `CULong' peek*,
    alloca- `ZPropSource' peekEnum*,
    `CString',
    `CULong' } -> `ZFSError' makeEnum #}

{#fun zfs_prop_get_userquota_int
  { fromZFSHandle `ZFSHandle',
    `CString',
    alloca- `CULong' peek* } -> `ZFSError' makeEnum #}

{#fun zfs_prop_get_userquota
  { fromZFSHandle `ZFSHandle',
    `CString',
    `CString',
    `CInt',
    `Bool' } -> `ZFSError' makeEnum #}

{#fun zfs_prop_get_written_int
  { fromZFSHandle `ZFSHandle',
    `CString',
    alloca- `CULong' peek* } -> `ZFSError' makeEnum #}

{#fun zfs_prop_get_written
  { fromZFSHandle `ZFSHandle',
    `CString',
    `CString',
    `CInt',
    `Bool' } -> `ZFSError' makeEnum #}

{-
{#fun zfs_prop_get_feature
  { fromZFSHandle `ZFSHandle',
    `CString',
    `CString',
    `CULong' } -> `ZFSError' makeEnum #}
-}

{#fun getprop_uint64
  { fromZFSHandle `ZFSHandle',
    `ZFSProp',
    id `Ptr CString' } -> `CULong' #}

{#fun zfs_prop_get_int
  { fromZFSHandle `ZFSHandle',
    `ZFSProp' } -> `CULong' #}

{#fun zfs_prop_inherit
  { fromZFSHandle `ZFSHandle',
    `CString',
    `Bool' } -> `ZFSError' makeEnum #}

{#fun zfs_prop_values
  { `ZFSProp' } -> `CString' #}

{#fun zfs_prop_is_string
  { `ZFSProp' } -> `Bool' #}

{#fun zfs_get_user_props
  { fromZFSHandle `ZFSHandle' } -> `NVList' #}

{-
{#fun zfs_get_recvd_props
  { fromZFSHandle `ZFSHandle' } -> `NVList' #}
-}

{#fun zfs_get_clones_nvl
  { fromZFSHandle `ZFSHandle' } -> `NVList' #}

{#pointer *zprop_list_t as ZPropList newtype #}

toZPropList = ZPropList . castPtr
fromZPropList (ZPropList p) = castPtr p

{#fun zfs_expand_proplist
  { fromZFSHandle `ZFSHandle',
    id `Ptr ZPropList',
    `Bool',
    `Bool' } -> `ZFSError' makeEnum #}

{#fun zfs_prune_proplist
  { fromZFSHandle `ZFSHandle',
    alloca- `CUChar' peek* } -> `()' #}

zfs_mountpoint_none = {#const ZFS_MOUNTPOINT_NONE #}
zfs_mountpoint_legacy = {#const ZFS_MOUNTPOINT_NONE #}

zfs_feature_disabled =  {#const ZFS_FEATURE_DISABLED #}
zfs_feature_enabled = {#const ZFS_FEATURE_ENABLED #}
zfs_feature_active = {#const ZFS_FEATURE_ACTIVE #}

zfs_unsupported_inactive = {#const ZFS_UNSUPPORTED_INACTIVE #}
zfs_unsupported_readonly = {#const ZFS_UNSUPPORTED_READONLY #}

-- zpool property management

{#fun zpool_expand_proplist
  { fromZPoolHandle `ZPoolHandle',
    id `Ptr ZPropList' } -> `ZFSError' makeEnum #}

{#fun zpool_prop_get_feature
  { fromZPoolHandle `ZPoolHandle',
    `CString',
    `CString',
    `CULong' } -> `ZFSError' makeEnum #}

{#fun zpool_prop_default_string
  { `ZPoolProp' } -> `CString' #}

{#fun zpool_prop_default_numeric
  { `ZPoolProp' } -> `CULong' #}

{#fun zpool_prop_column_name
  { `ZPoolProp' } -> `CString' #}

{#fun zpool_prop_align_right
  { `ZPoolProp' } -> `Bool' #}

-- Functions shared by zfs and zpool property management

{#fun zprop_iter
  { castFunPtr `FunPtr (ZPropFunction a)',
    castPtr `Ptr a',
    `Bool',
    `Bool',
    `ZFSType' } -> `ZFSError' makeEnum #}

{#fun zprop_get_list
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    id `Ptr ZPropList',
    `ZFSType' } -> `ZFSError' makeEnum #}

{#fun zprop_free_list
  { `ZPropList' } -> `()' #}

zfs_get_ncols = {#const ZFS_GET_NCOLS #}

{#enum zfs_get_column_t as ZFSGetColumn {underscoreToCase} deriving (Show, Eq) #}

{#pointer *zprop_get_cbdata_t as ZPropGetCbdata newtype #}

{#fun zprop_print_one_property
  { `CString',
    `ZPropGetCbdata',
    `CString',
    `CString',
    `ZPropSource',
    `CString',
    `CString' } -> `()' #}

type ZFSIterator a = ZFSHandle -> Ptr a -> IO CInt

foreign import ccall "wrapper"
  wrapZFSIterator :: ZFSIterator a -> IO (FunPtr (ZFSIterator a))

{#fun zfs_iter_root
  { fromLibZFSHandle `LibZFSHandle',
    castFunPtr `FunPtr (ZFSIterator a)',
    castPtr `Ptr a' } -> `CInt' #}

{#fun zfs_iter_children
  { fromZFSHandle `ZFSHandle',
    castFunPtr `FunPtr (ZFSIterator a)',
    castPtr `Ptr a' } -> `CInt' #}

{#fun zfs_iter_dependents
  { fromZFSHandle `ZFSHandle',
    `Bool',
    castFunPtr `FunPtr (ZFSIterator a)',
    castPtr `Ptr a' } -> `CInt' #}

{#fun zfs_iter_filesystems
  { fromZFSHandle `ZFSHandle',
    castFunPtr `FunPtr (ZFSIterator a)',
    castPtr `Ptr a' } -> `CInt' #}

{#fun zfs_iter_snapshots
  { fromZFSHandle `ZFSHandle',
    `Bool',
    castFunPtr `FunPtr (ZFSIterator a)',
    castPtr `Ptr a' } -> `CInt' #}

{#fun zfs_iter_snapshots_sorted
  { fromZFSHandle `ZFSHandle',
    castFunPtr `FunPtr (ZFSIterator a)',
    castPtr `Ptr a' } -> `CInt' #}

{#fun zfs_iter_snapspec
  { fromZFSHandle `ZFSHandle',
    `CString',
    castFunPtr `FunPtr (ZFSIterator a)',
    castPtr `Ptr a' } -> `CInt' #}

{#pointer *get_all_cb_t as GetAllCallback newtype #}

{#fun libzfs_add_handle
  { `GetAllCallback',
    fromZFSHandle `ZFSHandle' } -> `()' #}

{#fun libzfs_dataset_cmp
  { `Ptr ()',
    `Ptr ()' } -> `CInt' #}

-- Functions to create and destroy datasets

{#fun zfs_create
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `ZFSType',
    `NVList' } -> `ZFSError' makeEnum #}

{#fun zfs_create_ancestors
  { fromLibZFSHandle `LibZFSHandle',
    `CString' } -> `ZFSError' makeEnum #}

{#fun zfs_destroy
  { fromZFSHandle `ZFSHandle',
    `Bool' } -> `ZFSError' makeEnum #}

{#fun zfs_destroy_snaps
  { fromZFSHandle `ZFSHandle',
    `CString',
    `Bool' } -> `ZFSError' makeEnum #}

{#fun zfs_destroy_snaps_nvl
  { fromLibZFSHandle `LibZFSHandle',
    `NVList',
    `Bool' } -> `ZFSError' makeEnum #}

{#fun zfs_clone
  { fromZFSHandle `ZFSHandle',
    `CString',
    `NVList' } -> `ZFSError' makeEnum #}

{#fun zfs_snapshot
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `Bool',
    `NVList' } -> `ZFSError' makeEnum #}

{#fun zfs_snapshot_nvl
  { fromLibZFSHandle `LibZFSHandle',
    `NVList',
    `NVList' } -> `ZFSError' makeEnum #}

{#fun zfs_rollback
  { fromZFSHandle `ZFSHandle',
    fromZFSHandle `ZFSHandle',
    `Bool' } -> `ZFSError' makeEnum #}

{#fun zfs_rename
  { fromZFSHandle `ZFSHandle',
    `CString',
    `Bool',
    `Bool' } -> `ZFSError' makeEnum #}

{#pointer *sendflags_t as SendFlags newtype #}

type PrimitiveSnapfilter = FunPtr (Ptr () -> Ptr () -> IO CInt)
type SnapfilterCallback a = ZFSHandle -> Ptr a -> IO Bool

castSnapfilterCallback f = \handle val -> fmap fromBool $ f (toZFSHandle handle) (castPtr val)

{-
{#fun zfs_send
  { fromZFSHandle `ZFSHandle',
    `CString',
    `CString',
    `SendFlags',
    `CInt',
    id `PrimitiveSnapfilter',
    castPtr `Ptr a',
    id `Ptr NVList' } -> `ZFSError' makeEnum #}
-}

{#fun zfs_promote
  { fromZFSHandle `ZFSHandle' } -> `ZFSError' makeEnum #}

{#fun zfs_hold
  { fromZFSHandle `ZFSHandle',
    `CString',
    `CString',
    `Bool',
    `CInt' } -> `ZFSError' makeEnum #}

{#fun zfs_hold_nvl
  { fromZFSHandle `ZFSHandle',
    `CInt',
    `NVList' } -> `ZFSError' makeEnum #}

{#fun zfs_release
  { fromZFSHandle `ZFSHandle',
    `CString',
    `CString',
    `Bool' } -> `ZFSError' makeEnum #}

{#fun zfs_get_holds
  { fromZFSHandle `ZFSHandle',
    id `Ptr NVList' } -> `ZFSError' makeEnum #}

{#fun zvol_volsize_to_reservation
  { `CULong',
    `NVList' } -> `CULong' #}

type ZFSUserspaceCallback a = Ptr a -> CString -> CInt -> CULong -> IO CInt

{#enum zfs_userquota_prop_t as UserQuotaProp {underscoreToCase} deriving (Show, Eq) #}

{#fun zfs_userspace
  { fromZFSHandle `ZFSHandle',
    `UserQuotaProp',
    castFunPtr `FunPtr (ZFSUserspaceCallback a)',
    castPtr `Ptr a' } -> `ZFSError' makeEnum #}

{#fun zfs_get_fsacl
  { fromZFSHandle `ZFSHandle',
    id `Ptr NVList' } -> `ZFSError' makeEnum #}

{#fun zfs_set_fsacl
  { fromZFSHandle `ZFSHandle',
    `Bool',
    `NVList' } -> `ZFSError' makeEnum #}

{#pointer *recvflags_t as RecvFlags newtype #}

{#fun zfs_receive
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `RecvFlags',
    `CInt',
    `AVLTree' } -> `ZFSError' makeEnum #}

{#enum diff_flags_t as DiffFlags {underscoreToCase} deriving (Show, Eq)#}

{#fun zfs_show_diffs
  { fromZFSHandle `ZFSHandle',
    `CInt',
    `CString',
    `CString',
    `CInt' } -> `ZFSError' makeEnum #}

-- Miscellaneous functions
{#fun zfs_type_to_name
  { `ZFSType' } -> `CString' #}

{#fun zfs_refresh_properties
  { fromZFSHandle `ZFSHandle' } -> `()' #}

-- TODO check return type here
{#fun zfs_name_valid
  { `CString',
    `ZFSType' } -> `ZFSError' makeEnum #}

{#fun zfs_path_to_zhandle
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `ZFSType' } -> `ZFSHandle' toZFSHandle #}

{#fun zfs_dataset_exists
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `ZFSType' } -> `Bool' #}

{#fun zfs_spa_version
  { fromLibZFSHandle `LibZFSHandle',
    alloca- `CInt' peek* } -> `ZFSError' makeEnum #}

{#fun zfs_append_partition
  { `CString',
    `CULong' } -> `CInt' #}

{#fun zfs_resolve_shortname
  { `CString',
    `CString',
    `CULong' } -> `ZFSError' makeEnum #}

-- TODO check return type
{#fun zfs_strcmp_pathname
  { `CString',
    `CString',
    `CInt' } -> `CInt' #}

-- Mount support functions.

{#fun is_mounted
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    alloca- `CString' peek*} -> `Bool' toBool #}

{#fun zfs_is_mounted
  { fromZFSHandle `ZFSHandle',
    alloca- `CString' peek*} -> `Bool' toBool #}

{#fun zfs_mount
  { fromZFSHandle `ZFSHandle',
    `CString',
    `CInt' } -> `ZFSError' makeEnum #}

{#fun zfs_unmount
  { fromZFSHandle `ZFSHandle',
    `CString',
    `CInt' } -> `ZFSError' makeEnum #}

{#fun zfs_unmountall
  { fromZFSHandle `ZFSHandle',
    `CInt' } -> `ZFSError' makeEnum #}

-- Share support functions.
{#fun zfs_is_shared
  { fromZFSHandle `ZFSHandle' } -> `Bool' toBool #}

{#fun zfs_share
  { fromZFSHandle `ZFSHandle' } -> `ZFSError' makeEnum #}

{#fun zfs_unshare
  { fromZFSHandle `ZFSHandle' } -> `ZFSError' makeEnum #}

-- Protocol-specific share support functions.
{#fun zfs_is_shared_nfs
  { fromZFSHandle `ZFSHandle',
    alloca- `CString' peek*} -> `Bool' toBool #}

{#fun zfs_is_shared_smb
  { fromZFSHandle `ZFSHandle',
    alloca- `CString' peek*} -> `Bool' toBool #}

{#fun zfs_share_nfs
  { fromZFSHandle `ZFSHandle' } -> `ZFSError' makeEnum #}

{#fun zfs_share_smb
  { fromZFSHandle `ZFSHandle' } -> `ZFSError' makeEnum #}

{#fun zfs_unshare_nfs
  { fromZFSHandle `ZFSHandle',
    `CString' } -> `ZFSError' makeEnum #}

{#fun zfs_unshare_smb
  { fromZFSHandle `ZFSHandle',
    `CString' } -> `ZFSError' makeEnum #}

{#fun zfs_unshareall_nfs
  { fromZFSHandle `ZFSHandle' } -> `ZFSError' makeEnum #}

{#fun zfs_unshareall_smb
  { fromZFSHandle `ZFSHandle' } -> `ZFSError' makeEnum #}

{#fun zfs_unshareall_bypath
  { fromZFSHandle `ZFSHandle',
    `CString' } -> `ZFSError' makeEnum #}

{#fun zfs_unshareall
  { fromZFSHandle `ZFSHandle' } -> `ZFSError' makeEnum #}

{-
{#fun zfs_deleg_share_nfs
  { fromZFSHandle `ZFSHandle',
    `CString',
    `CString',
    `CString',
    `Ptr ()',
    `Ptr ()',
    `CInt',
    `ZFSShareOp' } -> `ZFSError' makeEnum #}
-}

-- Utility function to convert a number to a human-readable form.
{#fun zfs_nicenum
  { `CULong',
    `CString',
    `CULong' } -> `()' #}

{#fun zfs_nicestrtonum
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    alloca- `CULong' peek* } -> `ZFSError' makeEnum #}

-- Utility functions to run an external process. Probably overkill to support this?

-- stdout_verbose = {#const STDOUT_VERBOSE #}
-- stderr_verbose = {#const STDERR_VERBOSE #}

-- {#fun libzfs_run_process as

-- | Given a device or file, determine if it is part of a pool.
-- N.B. CInt should be PoolState, except whether it's valid depends on Bool below
{#fun zpool_in_use
  { fromLibZFSHandle `LibZFSHandle',
    `CInt',
    alloca- `CInt' peek*,
    alloca- `CString' peek*,
    alloca- `Bool' peekBool* } -> `ZFSError' makeEnum #}

-- Label manipulation
{#fun zpool_read_label
  { `CInt',
    id `Ptr NVList' } -> `ZFSError' makeEnum #}

{#fun zpool_clear_label
  { `CInt' } -> `ZFSError' makeEnum #}

-- Management interfaces for SMB ACL files.
{#fun zfs_smb_acl_add
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `CString',
    `CString' } -> `ZFSError' makeEnum #}

{#fun zfs_smb_acl_remove
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `CString',
    `CString' } -> `ZFSError' makeEnum #}

{#fun zfs_smb_acl_purge
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `CString' } -> `ZFSError' makeEnum #}

{#fun zfs_smb_acl_rename
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `CString',
    `CString',
    `CString' } -> `ZFSError' makeEnum #}

-- Mappings between vdev and FRU.
{-
{#fun libzfs_fru_refresh
  { fromLibZFSHandle `LibZFSHandle' } -> `()' #}

{#fun libzfs_fru_lookup
  { fromLibZFSHandle `LibZFSHandle',
    `CString' } -> `CString' #}

{#fun libzfs_fru_devpath
  { fromLibZFSHandle `LibZFSHandle',
    `CString' } -> `CString' #}

{#fun libzfs_fru_compare
  { fromLibZFSHandle `LibZFSHandle',
    `CString',
    `CString' } -> `Bool' toBool #}

{#fun libzfs_fru_notself
  { fromLibZFSHandle `LibZFSHandle',
    `CString' } -> `Bool' toBool #}

-- TODO check return type
{#fun zpool_fru_set
  { fromZPoolHandle `ZPoolHandle',
    `CULong',
    `CString' } -> `ZFSError' makeEnum #}
-}


