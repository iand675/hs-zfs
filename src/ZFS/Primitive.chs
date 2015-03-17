module ZFS.Primitive where
import Data.Word
import Foreign
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Marshal.Utils
{#import ZFS.Sys.AVL #}
{#import ZFS.Sys.FileSystem.ZFS #}
{#import ZFS.Sys.NVPair #}

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
{#enum zfs_error as ZfsError {underscoreToCase} deriving (Show, Eq)#}

{#pointer *zfs_perm_node as ZfsPermNode newtype #}
{#pointer *zfs_allow_node as ZfsAllowNode newtype #}
{#pointer *zfs_allow as ZfsAllow newtype #}

{#pointer *zfs_handle as ZfsHandle newtype #}
{#pointer *zpool_handle as ZpoolHandle newtype #}
{#pointer *libzfs_handle as LibZfsHandle newtype #}

toZfsHandle = ZfsHandle . castPtr
toZpoolHandle = ZpoolHandle . castPtr
toLibZfsHandle = LibZfsHandle . castPtr

fromZfsHandle (ZfsHandle p) = castPtr p
fromZpoolHandle (ZpoolHandle p) = castPtr p
fromLibZfsHandle (LibZfsHandle p) = castPtr p

makeEnum :: (Integral a, Enum b) => a -> b
makeEnum = toEnum . fromIntegral

peekEnum :: (Enum a) => Ptr CInt -> IO a
peekEnum = fmap makeEnum . peek

peekBool :: Ptr CInt -> IO Bool
peekBool = fmap toBool . peek

-- Library initialization
{#fun libzfs_init as ^
  {} -> `LibZfsHandle' toLibZfsHandle #}

{#fun libzfs_fini as ^
  {fromLibZfsHandle `LibZfsHandle'} -> `()' #}

{#fun zpool_get_handle as ^
  {fromZpoolHandle `ZpoolHandle'} -> `LibZfsHandle' toLibZfsHandle #}

{#fun zfs_get_handle as ^
  {fromZfsHandle `ZfsHandle'} -> `LibZfsHandle' toLibZfsHandle #}

{#fun libzfs_print_on_error as ^
  {fromLibZfsHandle `LibZfsHandle',
   `Bool'} -> `()' #}

-- | Concatenates all arguments into output string. Args: arg count, args, output string, output string length.
{#fun zfs_save_arguments as ^
  {`Int',
   id `Ptr CString',
   `CString',
   `Int'} -> `()' #}

-- | Add message to zpool log
{#fun zpool_log_history as ^
  { fromLibZfsHandle `LibZfsHandle'
  , id `CString' } -> `ZfsError' makeEnum #}

{#fun libzfs_errno as ^
  { fromLibZfsHandle `LibZfsHandle' } -> `ZfsError' makeEnum #}

{#fun libzfs_error_action as ^
  { fromLibZfsHandle `LibZfsHandle' } -> `CString' #}

{#fun libzfs_error_description as ^
  { fromLibZfsHandle `LibZfsHandle' } -> `CString' #}

{#fun libzfs_mnttab_init as ^
  { fromLibZfsHandle `LibZfsHandle' } -> `()' #}

{#fun libzfs_mnttab_fini as ^
  { fromLibZfsHandle `LibZfsHandle' } -> `()' #}

{#fun libzfs_mnttab_cache as ^
  { fromLibZfsHandle `LibZfsHandle',
    `Bool' } -> `()' #}

{#pointer *mnttab as MountTable newtype #}

{#fun libzfs_mnttab_find as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `MountTable' } -> `ZfsError' makeEnum #}

{#fun libzfs_mnttab_add as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `CString',
    `CString' } -> `()' #}

{#fun libzfs_mnttab_remove as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString' } -> `()' #}

-- Basic handle functions

{#fun zpool_open as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString' } -> `ZpoolHandle' toZpoolHandle #}

{#fun zpool_open_canfail as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString' } -> `ZpoolHandle' toZpoolHandle #}

{#fun zpool_close as ^
  { fromZpoolHandle `ZpoolHandle' } -> `()' #}

{#fun zpool_get_name as ^
  { fromZpoolHandle `ZpoolHandle' } -> `CString' #}

{#fun zpool_get_state as ^
  { fromZpoolHandle `ZpoolHandle' } -> `PoolState' makeEnum #}

-- {#fun zpool_state_to_name
-- zpool_pool_state_to_name

-- Iterate over all active pools in the system
{#fun zpool_free_handles as ^
  { fromLibZfsHandle `LibZfsHandle' } -> `()' #}

type ZpoolIterator a = ZpoolHandle -> Ptr a -> IO CInt

foreign import ccall "wrapper"
  wrapZPoolIterator :: ZpoolIterator a -> IO (FunPtr (ZpoolIterator a))

{#fun zpool_iter as ^
  { fromLibZfsHandle `LibZfsHandle',
    castFunPtr `FunPtr (ZpoolIterator a)',
    castPtr `Ptr a' } -> `CInt' #}

-- Functions to create and destroy pools
{#fun zpool_create as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `NVList',
    `NVList',
    `NVList' } -> `ZfsError' makeEnum #}

{#fun zpool_destroy as ^
  { fromZpoolHandle `ZpoolHandle',
    `CString' } -> `ZfsError' makeEnum #}

{#pointer *splitflags_t as SplitFlags newtype #}

-- Functions to manipulate pool and vdev state
{#fun zpool_scan as ^
  { fromZpoolHandle `ZpoolHandle',
    `PoolScanFunction' } -> `ZfsError' makeEnum #}

{#fun zpool_clear as ^
  { fromZpoolHandle `ZpoolHandle',
    `CString',
    `NVList' } -> `ZfsError' makeEnum #}

{#fun zpool_reguid as ^
  { fromZpoolHandle `ZpoolHandle' } -> `ZfsError' makeEnum #}

{#fun zpool_reopen as ^
  { fromZpoolHandle `ZpoolHandle' } -> `ZfsError' makeEnum #}

{#fun zpool_vdev_online as ^
  { fromZpoolHandle `ZpoolHandle',
    `CString',
    `Int',
    alloca- `VirtualDeviceState' peekEnum*} -> `ZfsError' makeEnum #}

{#fun zpool_vdev_offline as ^
  { fromZpoolHandle `ZpoolHandle',
    `CString',
    `Bool' } -> `ZfsError' makeEnum #}

{#fun zpool_vdev_attach as ^
  { fromZpoolHandle `ZpoolHandle',
    `CString',
    `CString',
    `NVList',
    `CInt' } -> `ZfsError' makeEnum #}

{#fun zpool_vdev_detach as ^
  { fromZpoolHandle `ZpoolHandle',
    `CString' } -> `ZfsError' makeEnum #}

{#fun zpool_vdev_remove as ^
  { fromZpoolHandle `ZpoolHandle',
    `CString' } -> `ZfsError' makeEnum #}

{-
TODO: Needs a wrapper function because split_flags_t is a bare struct
{#fun zpool_vdev_split
  { fromZpoolHandle `ZpoolHandle',
    `CString',
    `Ptr NVList',
    `NVList',
    % `SplitFlags' } -> `ZfsError' makeEnum #}
-}

{#fun zpool_vdev_fault as ^
  { fromZpoolHandle `ZpoolHandle',
    `CULong',
    `VirtualDeviceAuxilliaryState' } -> `ZfsError' makeEnum #}

{#fun zpool_vdev_degrade as ^
  { fromZpoolHandle `ZpoolHandle',
    `CULong',
    `VirtualDeviceAuxilliaryState' } -> `ZfsError' makeEnum #}

{#fun zpool_vdev_clear as ^
  { fromZpoolHandle `ZpoolHandle',
    `CULong' } -> `ZfsError' makeEnum #}

{#fun zpool_find_vdev as ^
  { fromZpoolHandle `ZpoolHandle',
    `CString',
    alloca- `Bool' peekBool*,
    alloca- `Bool' peekBool*,
    alloca- `Bool' peekBool*} -> `NVList' #}

{#fun zpool_find_vdev_by_physpath as ^
  { fromZpoolHandle `ZpoolHandle',
    `CString',
    alloca- `Bool' peekBool*,
    alloca- `Bool' peekBool*,
    alloca- `Bool' peekBool*} -> `NVList' #}

{#fun zpool_label_disk_wait as ^
  { `CString',
    `Int' } -> `ZfsError' makeEnum #}

{#fun zpool_label_disk as ^
  { fromLibZfsHandle `LibZfsHandle',
    fromZpoolHandle `ZpoolHandle',
    `CString' } -> `ZfsError' makeEnum #}

-- Functions to manage pool properties

{#fun zpool_set_prop as ^
  { fromZpoolHandle `ZpoolHandle',
    `CString',
    `CString' } -> `ZfsError' makeEnum #}

-- zpool_get_prop
-- zpool_get_prop_literal
-- zpool_get_prop_int
-- zpool_prop_to_name
-- zpool_prop_values

-- Pool health statistics
{#enum zpool_status_t as ZpoolStatus {underscoreToCase} deriving (Show, Eq)#}

-- zpool_get_status
-- zpool_import_status
-- zpool_dump_ddt

-- Statistics and configuration functions

{#fun zpool_get_config as ^
  { fromZpoolHandle `ZpoolHandle',
    id `Ptr NVList' } -> `NVList' #}

{#fun zpool_get_features as ^
  { fromZpoolHandle `ZpoolHandle' } -> `NVList' #}

{#fun zpool_refresh_stats as ^
  { fromZpoolHandle `ZpoolHandle',
    alloca- `Bool' peekBool* } -> `ZfsError' makeEnum #}

{#fun zpool_get_errlog as ^
  { fromZpoolHandle `ZpoolHandle',
    id `Ptr NVList' } -> `ZfsError' makeEnum #}

-- Import and export functions

{#fun zpool_export as ^
  { fromZpoolHandle `ZpoolHandle',
    `Bool',
    `CString' } -> `ZfsError' makeEnum #}

{#fun zpool_export_force as ^
  { fromZpoolHandle `ZpoolHandle',
    `CString' } -> `ZfsError' makeEnum #}

{#fun zpool_import as ^
  { fromLibZfsHandle `LibZfsHandle',
    `NVList',
    `CString',
    `CString' } -> `ZfsError' makeEnum #}

{#fun zpool_import_props as ^
  { fromLibZfsHandle `LibZfsHandle',
    `NVList',
    `CString',
    `NVList',
    `Int' } -> `ZfsError' makeEnum #}

{#fun zpool_print_unsup_feat as ^
  { `NVList' } -> `()' #}

-- Search for pools to import

{#pointer *importargs_t as ImportArgs newtype #}

{#fun zpool_search_import as ^
  { fromLibZfsHandle `LibZfsHandle',
    `ImportArgs' } -> `NVList' #}

-- Legacy pool search routines would go here, but skipping them for now.

-- Miscellaneous pool functions
{#pointer *zfs_cmd as ZfsCommand newtype #}

toZfsCommand = ZfsCommand . castPtr
fromZfsCommand (ZfsCommand p) = castPtr p

-- zfs_history_event_names
{#fun zpool_vdev_name as ^
  { fromLibZfsHandle `LibZfsHandle',
    fromZpoolHandle `ZpoolHandle',
    `NVList',
    `Bool' } -> `CString' #}

{#fun zpool_upgrade as ^
  { fromZpoolHandle `ZpoolHandle',
    `Word64' } -> `ZfsError' makeEnum #}

{#fun zpool_get_history as ^
  { fromZpoolHandle `ZpoolHandle',
    id `Ptr NVList' } -> `ZfsError' makeEnum #}

{#fun zpool_history_unpack as ^
  { `CString',
    `CULong',
    alloca- `CULong' peek*,
    id `Ptr (Ptr NVList)',
    alloca- `CUInt' peek*} -> `ZfsError' makeEnum #}

{#fun zpool_events_next as ^
  { fromLibZfsHandle `LibZfsHandle',
    id `Ptr NVList',
    alloca- `CInt' peek*,
    `CUInt',
    `CInt' } -> `ZfsError' makeEnum #}

{#fun zpool_events_clear as ^
  { fromLibZfsHandle `LibZfsHandle',
    alloca- `CInt' peek* } -> `ZfsError' makeEnum #}

{#fun zpool_events_seek as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CULong',
    `CInt' } -> `ZfsError' makeEnum #}

{#fun zpool_obj_to_path as ^
  { fromZpoolHandle `ZpoolHandle',
    `CULong',
    `CULong',
    `CString',
    `CULong' } -> `()' #}

{#fun zfs_ioctl as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CInt',
    `ZfsCommand' } -> `ZfsError' makeEnum #}

{#fun zpool_get_physpath as ^
  { fromZpoolHandle `ZpoolHandle',
    `CString',
    `CULong' } -> `ZfsError' makeEnum #}

{#fun zpool_explain_recover as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `CInt',
    `NVList' } -> `()' #}

-- Basic handle manipulations. These functions do not create or destroy the
-- underlying datasets, only the references to them.
{#fun zfs_open as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `CInt' } -> `ZfsHandle' toZfsHandle #}

{#fun zfs_handle_dup as ^
  { fromZfsHandle `ZfsHandle' } -> `ZfsHandle' toZfsHandle #}

{#fun zfs_close as ^
  { fromZfsHandle `ZfsHandle' } -> `()' #}

-- {#fun zfs_get_type 
{#fun zfs_get_name as ^
  { fromZfsHandle `ZfsHandle' } -> `CString' #}

{#fun zfs_get_pool_handle as ^
  { fromZfsHandle `ZfsHandle' } -> `ZpoolHandle' toZpoolHandle #}

-- Property management functions. Some functions are shared with the kernel,
-- and are found in sys/fs/zfs.h

-- zfs dataset property management
{#fun zfs_prop_default_string as ^
  { `ZfsProp' } -> `CString' #}

{#fun zfs_prop_default_numeric as ^
  { `ZfsProp' } -> `CULong' #}

{#fun zfs_prop_column_name as ^
  { `ZfsProp' } -> `CString' #}

{#fun zfs_prop_align_right as ^
  { `ZfsProp' } -> `Bool' #}

{#fun zfs_valid_proplist as ^
  { fromLibZfsHandle `LibZfsHandle',
    `ZfsType',
    `NVList',
    `CULong',
    fromZfsHandle `ZfsHandle',
    `CString' } -> `NVList' #}

{#fun zfs_prop_to_name as ^
  { `ZfsProp' } -> `CString' #}

{#fun zfs_prop_set as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `CString' } -> `ZfsError' makeEnum #}

{#fun zfs_prop_get as ^
  { fromZfsHandle `ZfsHandle',
    `ZfsProp',
    `CString',
    `CULong',
    alloca- `ZpropSource',
    `CString',
    `CULong',
    `Bool'} -> `ZfsError' makeEnum #}

{#fun zfs_prop_get_recvd as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `CString',
    `CULong',
    `Bool' } -> `ZfsError' makeEnum #}

{#fun zfs_prop_get_numeric as ^
  { fromZfsHandle `ZfsHandle',
    `ZpropSource',
    alloca- `CULong' peek*,
    alloca- `ZpropSource' peekEnum*,
    `CString',
    `CULong' } -> `ZfsError' makeEnum #}

{#fun zfs_prop_get_userquota_int as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    alloca- `CULong' peek* } -> `ZfsError' makeEnum #}

{#fun zfs_prop_get_userquota as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `CString',
    `CInt',
    `Bool' } -> `ZfsError' makeEnum #}

{#fun zfs_prop_get_written_int as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    alloca- `CULong' peek* } -> `ZfsError' makeEnum #}

{#fun zfs_prop_get_written as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `CString',
    `CInt',
    `Bool' } -> `ZfsError' makeEnum #}

{-
{#fun zfs_prop_get_feature as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `CString',
    `CULong' } -> `ZfsError' makeEnum #}
-}

{#fun getprop_uint64 as ^
  { fromZfsHandle `ZfsHandle',
    `ZfsProp',
    id `Ptr CString' } -> `CULong' #}

{#fun zfs_prop_get_int as ^
  { fromZfsHandle `ZfsHandle',
    `ZfsProp' } -> `CULong' #}

{#fun zfs_prop_inherit as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `Bool' } -> `ZfsError' makeEnum #}

{#fun zfs_prop_values as ^
  { `ZfsProp' } -> `CString' #}

{#fun zfs_prop_is_string as ^
  { `ZfsProp' } -> `Bool' #}

{#fun zfs_get_user_props as ^
  { fromZfsHandle `ZfsHandle' } -> `NVList' #}

{-
{#fun zfs_get_recvd_props as ^
  { fromZfsHandle `ZfsHandle' } -> `NVList' #}
-}

{#fun zfs_get_clones_nvl as ^
  { fromZfsHandle `ZfsHandle' } -> `NVList' #}

{#pointer *zprop_list_t as ZpropList newtype #}

toZpropList = ZpropList . castPtr
fromZpropList (ZpropList p) = castPtr p

{#fun zfs_expand_proplist as ^
  { fromZfsHandle `ZfsHandle',
    id `Ptr ZpropList',
    `Bool',
    `Bool' } -> `ZfsError' makeEnum #}

{#fun zfs_prune_proplist as ^
  { fromZfsHandle `ZfsHandle',
    alloca- `CUChar' peek* } -> `()' #}

zfs_mountpoint_none = {#const ZFS_MOUNTPOINT_NONE #}
zfs_mountpoint_legacy = {#const ZFS_MOUNTPOINT_NONE #}

zfs_feature_disabled =  {#const ZFS_FEATURE_DISABLED #}
zfs_feature_enabled = {#const ZFS_FEATURE_ENABLED #}
zfs_feature_active = {#const ZFS_FEATURE_ACTIVE #}

zfs_unsupported_inactive = {#const ZFS_UNSUPPORTED_INACTIVE #}
zfs_unsupported_readonly = {#const ZFS_UNSUPPORTED_READONLY #}

-- zpool property management

{#fun zpool_expand_proplist as ^
  { fromZpoolHandle `ZpoolHandle',
    id `Ptr ZpropList' } -> `ZfsError' makeEnum #}

{#fun zpool_prop_get_feature as ^
  { fromZpoolHandle `ZpoolHandle',
    `CString',
    `CString',
    `CULong' } -> `ZfsError' makeEnum #}

{#fun zpool_prop_default_string as ^
  { `ZpoolProp' } -> `CString' #}

{#fun zpool_prop_default_numeric as ^
  { `ZpoolProp' } -> `CULong' #}

{#fun zpool_prop_column_name as ^
  { `ZpoolProp' } -> `CString' #}

{#fun zpool_prop_align_right as ^
  { `ZpoolProp' } -> `Bool' #}

-- Functions shared by zfs and zpool property management

{#fun zprop_iter as ^
  { castFunPtr `FunPtr (ZpropFunction a)',
    castPtr `Ptr a',
    `Bool',
    `Bool',
    `ZfsType' } -> `ZfsError' makeEnum #}

{#fun zprop_get_list as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    id `Ptr ZpropList',
    `ZfsType' } -> `ZfsError' makeEnum #}

{#fun zprop_free_list as ^
  { `ZpropList' } -> `()' #}

zfs_get_ncols = {#const ZFS_GET_NCOLS #}

{#enum zfs_get_column_t as ZfsGetColumn {underscoreToCase} deriving (Show, Eq) #}

{#pointer *zprop_get_cbdata_t as ZpropGetCbdata newtype #}

{#fun zprop_print_one_property as ^
  { `CString',
    `ZpropGetCbdata',
    `CString',
    `CString',
    `ZpropSource',
    `CString',
    `CString' } -> `()' #}

type ZfsIterFunction a = ZfsHandle -> Ptr a -> CInt

{#fun zfs_iter_root as ^
  { fromLibZfsHandle `LibZfsHandle',
    castFunPtr `FunPtr (ZfsIterFunction a)',
    castPtr `Ptr a' } -> `ZfsError' makeEnum #}

{#fun zfs_iter_children as ^
  { fromZfsHandle `ZfsHandle',
    castFunPtr `FunPtr (ZfsIterFunction a)',
    castPtr `Ptr a' } -> `ZfsError' makeEnum #}

{#fun zfs_iter_dependents as ^
  { fromZfsHandle `ZfsHandle',
    `Bool',
    castFunPtr `FunPtr (ZfsIterFunction a)',
    castPtr `Ptr a' } -> `ZfsError' makeEnum #}

{#fun zfs_iter_filesystems as ^
  { fromZfsHandle `ZfsHandle',
    castFunPtr `FunPtr (ZfsIterFunction a)',
    castPtr `Ptr a' } -> `ZfsError' makeEnum #}

{#fun zfs_iter_snapshots as ^
  { fromZfsHandle `ZfsHandle',
    `Bool',
    castFunPtr `FunPtr (ZfsIterFunction a)',
    castPtr `Ptr a' } -> `ZfsError' makeEnum #}

{#fun zfs_iter_snapshots_sorted as ^
  { fromZfsHandle `ZfsHandle',
    castFunPtr `FunPtr (ZfsIterFunction a)',
    castPtr `Ptr a' } -> `ZfsError' makeEnum #}

{#fun zfs_iter_snapspec as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    castFunPtr `FunPtr (ZfsIterFunction a)',
    castPtr `Ptr a' } -> `ZfsError' makeEnum #}

{#pointer *get_all_cb_t as GetAllCallback newtype #}

{#fun libzfs_add_handle as ^
  { `GetAllCallback',
    fromZfsHandle `ZfsHandle' } -> `()' #}

{#fun libzfs_dataset_cmp as ^
  { `Ptr ()',
    `Ptr ()' } -> `CInt' #}

-- Functions to create and destroy datasets

{#fun zfs_create as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `ZfsType',
    `NVList' } -> `ZfsError' makeEnum #}

{#fun zfs_create_ancestors as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString' } -> `ZfsError' makeEnum #}

{#fun zfs_destroy as ^
  { fromZfsHandle `ZfsHandle',
    `Bool' } -> `ZfsError' makeEnum #}

{#fun zfs_destroy_snaps as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `Bool' } -> `ZfsError' makeEnum #}

{#fun zfs_destroy_snaps_nvl as ^
  { fromLibZfsHandle `LibZfsHandle',
    `NVList',
    `Bool' } -> `ZfsError' makeEnum #}

{#fun zfs_clone as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `NVList' } -> `ZfsError' makeEnum #}

{#fun zfs_snapshot as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `Bool',
    `NVList' } -> `ZfsError' makeEnum #}

{#fun zfs_snapshot_nvl as ^
  { fromLibZfsHandle `LibZfsHandle',
    `NVList',
    `NVList' } -> `ZfsError' makeEnum #}

{#fun zfs_rollback as ^
  { fromZfsHandle `ZfsHandle',
    fromZfsHandle `ZfsHandle',
    `Bool' } -> `ZfsError' makeEnum #}

{#fun zfs_rename as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `Bool',
    `Bool' } -> `ZfsError' makeEnum #}

{#pointer *sendflags_t as SendFlags newtype #}

type PrimitiveSnapfilter = FunPtr (Ptr () -> Ptr () -> IO CInt)
type SnapfilterCallback a = ZfsHandle -> Ptr a -> IO Bool

castSnapfilterCallback f = \handle val -> fmap fromBool $ f (toZfsHandle handle) (castPtr val)

{-
{#fun zfs_send as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `CString',
    `SendFlags',
    `CInt',
    id `PrimitiveSnapfilter',
    castPtr `Ptr a',
    id `Ptr NVList' } -> `ZfsError' makeEnum #}
-}

{#fun zfs_promote as ^
  { fromZfsHandle `ZfsHandle' } -> `ZfsError' makeEnum #}

{#fun zfs_hold as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `CString',
    `Bool',
    `CInt' } -> `ZfsError' makeEnum #}

{#fun zfs_hold_nvl as ^
  { fromZfsHandle `ZfsHandle',
    `CInt',
    `NVList' } -> `ZfsError' makeEnum #}

{#fun zfs_release as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `CString',
    `Bool' } -> `ZfsError' makeEnum #}

{#fun zfs_get_holds as ^
  { fromZfsHandle `ZfsHandle',
    id `Ptr NVList' } -> `ZfsError' makeEnum #}

{#fun zvol_volsize_to_reservation as ^
  { `CULong',
    `NVList' } -> `CULong' #}

type ZfsUserspaceCallback a = Ptr a -> CString -> CInt -> CULong -> IO CInt

{#enum zfs_userquota_prop_t as UserQuotaProp {underscoreToCase} deriving (Show, Eq) #}

{#fun zfs_userspace as ^
  { fromZfsHandle `ZfsHandle',
    `UserQuotaProp',
    castFunPtr `FunPtr (ZfsUserspaceCallback a)',
    castPtr `Ptr a' } -> `ZfsError' makeEnum #}

{#fun zfs_get_fsacl as ^
  { fromZfsHandle `ZfsHandle',
    id `Ptr NVList' } -> `ZfsError' makeEnum #}

{#fun zfs_set_fsacl as ^
  { fromZfsHandle `ZfsHandle',
    `Bool',
    `NVList' } -> `ZfsError' makeEnum #}

{#pointer *recvflags_t as RecvFlags newtype #}

{#fun zfs_receive as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `RecvFlags',
    `CInt',
    `AVLTree' } -> `ZfsError' makeEnum #}

{#enum diff_flags_t as DiffFlags {underscoreToCase} deriving (Show, Eq)#}

{#fun zfs_show_diffs as ^
  { fromZfsHandle `ZfsHandle',
    `CInt',
    `CString',
    `CString',
    `CInt' } -> `ZfsError' makeEnum #}

-- Miscellaneous functions
{#fun zfs_type_to_name as ^
  { `ZfsType' } -> `CString' #}

{#fun zfs_refresh_properties as ^
  { fromZfsHandle `ZfsHandle' } -> `()' #}

-- TODO check return type here
{#fun zfs_name_valid as ^
  { `CString',
    `ZfsType' } -> `ZfsError' makeEnum #}

{#fun zfs_path_to_zhandle as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `ZfsType' } -> `ZfsHandle' toZfsHandle #}

{#fun zfs_dataset_exists as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `ZfsType' } -> `Bool' #}

{#fun zfs_spa_version as ^
  { fromLibZfsHandle `LibZfsHandle',
    alloca- `CInt' peek* } -> `ZfsError' makeEnum #}

{#fun zfs_append_partition as ^
  { `CString',
    `CULong' } -> `ZfsError' makeEnum #}

{#fun zfs_resolve_shortname as ^
  { `CString',
    `CString',
    `CULong' } -> `ZfsError' makeEnum #}

-- TODO check return type
{#fun zfs_strcmp_pathname as ^
  { `CString',
    `CString',
    `CInt' } -> `CInt' #}

-- Mount support functions.

{#fun is_mounted as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    id `Ptr CString' } -> `Bool' toBool #}

{#fun zfs_is_mounted as ^
  { fromZfsHandle `ZfsHandle',
    id `Ptr CString' } -> `Bool' toBool #}

{#fun zfs_mount as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `CInt' } -> `ZfsError' makeEnum #}

{#fun zfs_unmount as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `CInt' } -> `ZfsError' makeEnum #}

{#fun zfs_unmountall as ^
  { fromZfsHandle `ZfsHandle',
    `CInt' } -> `ZfsError' makeEnum #}

-- Share support functions.
{#fun zfs_is_shared as ^
  { fromZfsHandle `ZfsHandle' } -> `Bool' toBool #}

{#fun zfs_share as ^
  { fromZfsHandle `ZfsHandle' } -> `ZfsError' makeEnum #}

{#fun zfs_unshare as ^
  { fromZfsHandle `ZfsHandle' } -> `ZfsError' makeEnum #}

-- Protocol-specific share support functions.
{#fun zfs_is_shared_nfs as ^
  { fromZfsHandle `ZfsHandle',
    id `Ptr CString' } -> `Bool' toBool #}

{#fun zfs_is_shared_smb as ^
  { fromZfsHandle `ZfsHandle',
    id `Ptr CString' } -> `Bool' toBool #}

{#fun zfs_share_nfs as ^
  { fromZfsHandle `ZfsHandle' } -> `ZfsError' makeEnum #}

{#fun zfs_share_smb as ^
  { fromZfsHandle `ZfsHandle' } -> `ZfsError' makeEnum #}

{#fun zfs_unshare_nfs as ^
  { fromZfsHandle `ZfsHandle',
    `CString' } -> `ZfsError' makeEnum #}

{#fun zfs_unshare_smb as ^
  { fromZfsHandle `ZfsHandle',
    `CString' } -> `ZfsError' makeEnum #}

{#fun zfs_unshareall_nfs as ^
  { fromZfsHandle `ZfsHandle' } -> `ZfsError' makeEnum #}

{#fun zfs_unshareall_smb as ^
  { fromZfsHandle `ZfsHandle' } -> `ZfsError' makeEnum #}

{#fun zfs_unshareall_bypath as ^
  { fromZfsHandle `ZfsHandle',
    `CString' } -> `ZfsError' makeEnum #}

{#fun zfs_unshareall as ^
  { fromZfsHandle `ZfsHandle' } -> `ZfsError' makeEnum #}

{-
{#fun zfs_deleg_share_nfs as ^
  { fromZfsHandle `ZfsHandle',
    `CString',
    `CString',
    `CString',
    `Ptr ()',
    `Ptr ()',
    `CInt',
    `ZfsShareOp' } -> `ZfsError' makeEnum #}
-}

-- Utility function to convert a number to a human-readable form.
{#fun zfs_nicenum as ^
  { `CULong',
    `CString',
    `CULong' } -> `()' #}

{#fun zfs_nicestrtonum as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    alloca- `CULong' peek* } -> `ZfsError' makeEnum #}

-- Utility functions to run an external process. Probably overkill to support this?

-- stdout_verbose = {#const STDOUT_VERBOSE #}
-- stderr_verbose = {#const STDERR_VERBOSE #}

-- {#fun libzfs_run_process as

-- Given a device or file, determine if it is part of a pool.
{#fun zpool_in_use as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CInt',
    alloca- `PoolState' peekEnum*,
    id `Ptr CString',
    alloca- `Bool' peekBool* } -> `ZfsError' makeEnum #}

-- Label manipulation
{#fun zpool_read_label as ^
  { `CInt',
    id `Ptr NVList' } -> `ZfsError' makeEnum #}

{#fun zpool_clear_label as ^
  { `CInt' } -> `ZfsError' makeEnum #}

-- Management interfaces for SMB ACL files.
{#fun zfs_smb_acl_add as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `CString',
    `CString' } -> `ZfsError' makeEnum #}

{#fun zfs_smb_acl_remove as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `CString',
    `CString' } -> `ZfsError' makeEnum #}

{#fun zfs_smb_acl_purge as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `CString' } -> `ZfsError' makeEnum #}

{#fun zfs_smb_acl_rename as ^
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `CString',
    `CString',
    `CString' } -> `ZfsError' makeEnum #}

-- Mappings between vdev and FRU.
{-
{#fun libzfs_fru_refresh
  { fromLibZfsHandle `LibZfsHandle' } -> `()' #}

{#fun libzfs_fru_lookup
  { fromLibZfsHandle `LibZfsHandle',
    `CString' } -> `CString' #}

{#fun libzfs_fru_devpath
  { fromLibZfsHandle `LibZfsHandle',
    `CString' } -> `CString' #}

{#fun libzfs_fru_compare
  { fromLibZfsHandle `LibZfsHandle',
    `CString',
    `CString' } -> `Bool' toBool #}

{#fun libzfs_fru_notself
  { fromLibZfsHandle `LibZfsHandle',
    `CString' } -> `Bool' toBool #}

-- TODO check return type
{#fun zpool_fru_set
  { fromZpoolHandle `ZpoolHandle',
    `CULong',
    `CString' } -> `ZfsError' makeEnum #}
-}
