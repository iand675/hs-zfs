module ZFS.Sys.NVPair where
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Utils

-- Includes sys/nvpair.h too
#define HAVE_IOCTL_IN_SYS_IOCTL_H
#include <libnvpair.h>

{#enum data_type_t as DataType {underscoreToCase} deriving (Eq)#}

{#pointer *nvpair_t as NVPair newtype #}
{#pointer *nvlist_t as NVList newtype #}

nv_version = {#const NV_VERSION #}

nv_encode_native = {#const NV_ENCODE_NATIVE #}
nv_encode_xor = {#const NV_ENCODE_XDR #}

nv_unique_name = {#const NV_UNIQUE_NAME #}
nv_unique_name_type = {#const NV_UNIQUE_NAME_TYPE #}

nv_flag_noentok = {#const NV_FLAG_NOENTOK #}


