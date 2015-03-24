module ZFS.Primitive.FS where
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Utils

#define HAVE_IOCTL_IN_SYS_IOCTL_H
#include <sys/fs/zfs.h>

{#context lib = "zfs" #}

{#enum zfs_type_t as ZFSType {underscoreToCase} deriving (Eq)#}
{#enum dmu_objset_type_t as DMUObjSetType {underscoreToCase} deriving (Eq)#}

zfs_type_dataset = [ZfsTypeFilesystem, ZfsTypeVolume, ZfsTypeSnapshot]

zap_maxnamelen = {#const ZAP_MAXNAMELEN #}
zap_maxvaluelen = {#const ZAP_MAXVALUELEN #}
zap_oldmaxvaluelen = {#const ZAP_OLDMAXVALUELEN #}

{#enum zfs_prop_t as ZFSProp {underscoreToCase} deriving (Eq)#}
{#enum zfs_userquota_prop_t as ZFSUserquotaProp {underscoreToCase} deriving (Eq)#}

-- zfs_userquota_prop_prefixes

{#enum zpool_prop_t as ZPoolProp {underscoreToCase} deriving (Eq)#}

zprop_max_comment = {#const ZPROP_MAX_COMMENT #}

zprop_cont = {#const ZPROP_CONT #}
zprop_inval = {#const ZPROP_INVAL #}

zprop_value = {#const ZPROP_VALUE #}
zprop_source = {#const ZPROP_SOURCE #}

{#enum zprop_source_t as ZPropSource {underscoreToCase} deriving (Eq) #}

zprop_src_all = {#const ZPROP_SRC_ALL #}

zprop_source_val_recvd = {#const ZPROP_SOURCE_VAL_RECVD #}
zprop_n_more_errors = {#const ZPROP_N_MORE_ERRORS #}

zprop_has_recvd = {#const ZPROP_HAS_RECVD #}

{#enum zprop_errflags_t as ZPropErrorFlags {underscoreToCase} deriving (Eq) #}

type ZPropFunction a = CInt -> Ptr a -> CInt

zpool_rootfs_props = {#const ZPOOL_ROOTFS_PROPS #}

-- Lots of stuff skipped here

{#enum zfs_share_op_t as ZFSShareOp {underscoreToCase} deriving (Show, Eq) #}

-- Lots of stuff skipped here

{#enum pool_state_t as PoolState {underscoreToCase} deriving (Show, Eq)#}

{#enum pool_scan_func_t as PoolScanFunction {underscoreToCase} deriving (Eq) #}

-- Lots of stuff skipped here

{#enum vdev_state_t as VirtualDeviceState {underscoreToCase} deriving (Eq) #}

vdevStateOnline = VdevStateHealthy

{#enum vdev_aux_t as VirtualDeviceAuxilliaryState {underscoreToCase} deriving (Eq)#}

-- Lots of stuff skipped here

{#enum define OnlineFlag {ZFS_ONLINE_CHECKREMOVE as CheckRemove, ZFS_ONLINE_UNSPARE as Unspare, ZFS_ONLINE_FORCEFAULT as ForceFault, ZFS_ONLINE_EXPAND as Expand} deriving (Show, Eq) #}

