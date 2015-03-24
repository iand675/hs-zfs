{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module ZFS.Primitive.NVPair where
import qualified Data.ByteString.Char8 as C
import Data.Bits
import Data.Int
import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Marshal.Utils
import Foreign.Storable

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

deriving instance Storable NVPair
deriving instance Storable NVList
-- * List management

{#fun nvlist_alloc
  { alloca- `NVList' peek*,
    `CUInt',
    `CInt' } -> `CInt' #}

{#fun nvlist_free
  { `NVList' } -> `()' #}

{#fun nvlist_size
  { `NVList',
    alloca- `CULong' peek*,
    `CInt' } -> `CInt' #}

{#fun nvlist_pack
  { `NVList',
    alloca- `CString' peek*,
    alloca- `CULong' peek*,
    `CInt',
    `CInt' } -> `CInt' #}

{#fun nvlist_unpack
  { `CString',
    `CUInt',
    alloca- `NVList' peek*,
    `CInt' } -> `CInt' #}

{#fun nvlist_dup
  { `NVList',
    alloca- `NVList' peek*,
    `CInt' } -> `CInt' #}

{#fun nvlist_merge
  { `NVList',
    `NVList',
    `CInt' } -> `CInt' #}

{#fun nvlist_nvflag
  { `NVList' } -> `CUInt' #}

-- xalloc
-- xpcak
-- xunpack
-- xdup
-- nvlist_lookup_nv_alloc

{#fun nvlist_add_nvpair
  { `NVList',
    `NVPair' } -> `CInt' #}

{#fun nvlist_add_boolean
  { `NVList',
    `CString' } -> `CInt' #}

{#fun nvlist_add_boolean_value
  { `NVList',
    `CString',
    `Bool' } -> `CInt' #}

{#fun nvlist_add_byte
  { `NVList',
    `CString',
    fromIntegral `CChar' } -> `CInt' #}

{#fun nvlist_add_int8
  { `NVList',
    `CString',
    fromIntegral `CSChar' } -> `CInt' #}

{#fun nvlist_add_uint8
  { `NVList',
    `CString',
    fromIntegral `CUChar' } -> `CInt' #}

{#fun nvlist_add_int16
  { `NVList',
    `CString',
    `CShort' } -> `CInt' #}

{#fun nvlist_add_uint16
  { `NVList',
    `CString',
    `CUShort' } -> `CInt' #}

{#fun nvlist_add_int32
  { `NVList',
    `CString',
    `CInt' } -> `CInt' #}

{#fun nvlist_add_uint32
  { `NVList',
    `CString',
    `CUInt' } -> `CInt' #}

{#fun nvlist_add_int64
  { `NVList',
    `CString',
    `CLong' } -> `CInt' #}

{#fun nvlist_add_uint64
  { `NVList',
    `CString',
    `CULong' } -> `CInt' #}

{#fun nvlist_add_string
  { `NVList',
    `CString',
    `CString' } -> `CInt' #}

{#fun nvlist_add_nvlist
  { `NVList',
    `CString',
    `NVList' } -> `CInt' #}

{#fun nvlist_add_boolean_array
  { `NVList',
    `CString',
    castPtr `Ptr Bool',
    `CUInt' } -> `CInt' #}

{#fun nvlist_add_byte_array
  { `NVList',
    `CString',
    id `Ptr CUChar',
    `CUInt' } -> `CInt' #}

{#fun nvlist_add_int8_array
  { `NVList',
    `CString',
    id `Ptr CInt',
    `CUInt' } -> `CInt' #}

{#fun nvlist_add_uint8_array
  { `NVList',
    `CString',
    id `Ptr CUChar',
    `CUInt' } -> `CInt' #}

{#fun nvlist_add_int16_array
  { `NVList',
    `CString',
    id `Ptr CInt',
    `CUInt' } -> `CInt' #}

{#fun nvlist_add_uint16_array
  { `NVList',
    `CString',
    id `Ptr CUShort',
    `CUInt' } -> `CInt' #}

{#fun nvlist_add_int32_array
  { `NVList',
    `CString',
    id `Ptr CInt',
    `CUInt' } -> `CInt' #}

{#fun nvlist_add_uint32_array
  { `NVList',
    `CString',
    id `Ptr CUInt',
    `CUInt' } -> `CInt' #}

{#fun nvlist_add_int64_array
  { `NVList',
    `CString',
    id `Ptr CInt',
    `CUInt' } -> `CInt' #}

{#fun nvlist_add_uint64_array
  { `NVList',
    `CString',
    id `Ptr CULong',
    `CUInt' } -> `CInt' #}

{#fun nvlist_add_string_array
  { `NVList',
    `CString',
    id `Ptr CString',
    `CUInt' } -> `CInt' #}

{#fun nvlist_add_nvlist_array
  { `NVList',
    `CString',
    id `Ptr NVList',
    `CUInt' } -> `CInt' #}

-- int nvlist_add_hrtime(nvlist_t *, const char *, hrtime_t);

#if !defined(_KERNEL)
{#fun nvlist_add_double
  { `NVList',
    `CString',
    `CDouble' } -> `CInt' #}
#endif

{#fun nvlist_remove
  { `NVList',
    `CString',
    `DataType' } -> `CInt' #}

{#fun nvlist_remove_all
  { `NVList',
    `CString' } -> `CInt' #}

{#fun nvlist_remove_nvpair
  { `NVList',
    `NVPair' } -> `CInt' #}

{#fun nvlist_lookup_boolean
  { `NVList',
    `CString' } -> `CInt' #}

peekBool = fmap toBool . peek

{#fun nvlist_lookup_boolean_value
  { `NVList',
    `CString',
    alloca- `Bool' peekBool* } -> `CInt' #}

{#fun nvlist_lookup_byte
  { `NVList',
    `CString',
    alloca- `CUChar' peek* } -> `CInt' #}

{#fun nvlist_lookup_int8
  { `NVList',
    `CString',
    alloca- `CInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_uint8
  { `NVList',
    `CString',
    alloca- `CUChar' peek* } -> `CInt' #}

{#fun nvlist_lookup_int16
  { `NVList',
    `CString',
    alloca- `CInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_uint16
  { `NVList',
    `CString',
    alloca- `CUShort' peek* } -> `CInt' #}

{#fun nvlist_lookup_int32
  { `NVList',
    `CString',
    alloca- `CInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_uint32
  { `NVList',
    `CString',
    alloca- `CUInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_int64
  { `NVList',
    `CString',
    alloca- `CInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_uint64
  { `NVList',
    `CString',
    alloca- `CULong' peek* } -> `CInt' #}

{#fun nvlist_lookup_string
  { `NVList',
    `CString',
    alloca- `CString' peek*  } -> `CInt' #}

{#fun nvlist_lookup_nvlist
  { `NVList',
    `CString',
    alloca- `NVList' peek*  } -> `CInt' #}

{#fun nvlist_lookup_boolean_array
  { `NVList',
    `CString',
    alloca- `Ptr CInt' peek*,
    alloca- `CUInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_byte_array
  { `NVList',
    `CString',
    alloca- `Ptr CUChar' peek*,
    alloca- `CUInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_int8_array
  { `NVList',
    `CString',
    alloca- `Ptr CInt' peek*,
    alloca- `CUInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_uint8_array
  { `NVList',
    `CString',
    alloca- `Ptr CUChar' peek*,
    alloca- `CUInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_int16_array
  { `NVList',
    `CString',
    alloca- `Ptr CInt' peek*,
    alloca- `CUInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_uint16_array
  { `NVList',
    `CString',
    alloca- `Ptr CUShort' peek*,
    alloca- `CUInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_int32_array
  { `NVList',
    `CString',
    alloca- `Ptr CInt' peek*,
    alloca- `CUInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_uint32_array
  { `NVList',
    `CString',
    alloca- `Ptr CUInt' peek*,
    alloca- `CUInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_int64_array
  { `NVList',
    `CString',
    alloca- `Ptr CInt' peek*,
    alloca- `CUInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_uint64_array
  { `NVList',
    `CString',
    alloca- `Ptr CULong' peek*,
    alloca- `CUInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_string_array
  { `NVList',
    `CString',
    alloca- `Ptr CString' peek*,
    alloca- `CUInt' peek* } -> `CInt' #}

{#fun nvlist_lookup_nvlist_array
  { `NVList',
    `CString',
    alloca- `Ptr NVList' peek*,
    alloca- `CUInt' peek* } -> `CInt' #}

-- int nvlist_lookup_hrtime(nvlist_t *, const char *, hrtime_t *);

#if !defined(_KERNEL)
{#fun nvlist_lookup_double
  { `NVList',
    `CString',
    alloca- `Ptr Double' } -> `CInt' #}
#endif

{#fun nvlist_lookup_nvpair
  { `NVList',
    `CString',
    alloca- `NVPair' peek* } -> `CInt' #}

{#fun nvlist_lookup_nvpair_embedded_index
  { `NVList',
    `CString',
    alloca- `NVPair' peek*,
    alloca- `CInt' peek*,
    alloca- `CString' peek* } -> `CInt' #}

{#fun nvlist_exists
  { `NVList',
    `CString' } -> `Bool' #}

{#fun nvlist_empty
  { `NVList' } -> `Bool' #}

-- * Processing nvpair
{#fun nvlist_next_nvpair
  { `NVList',
    `NVPair' } -> `NVPair' #}

{#fun nvlist_prev_nvpair
  { `NVList',
    `NVPair' } -> `NVPair' #}

{#fun nvpair_name
  { `NVPair' } -> `CString' #}

{#fun nvpair_type
  { `NVPair' } -> `DataType' #}

{#fun nvpair_type_is_array
  { `NVPair' } -> `CInt' #}

{#fun nvpair_value_boolean_value
  { `NVPair',
    id `Ptr CInt' } -> `CInt' #}

{#fun nvpair_value_byte
  { `NVPair',
    id `Ptr CUChar' } -> `CInt' #}

{#fun nvpair_value_int8
  { `NVPair',
    id `Ptr CInt' } -> `CInt' #}

{#fun nvpair_value_uint8
  { `NVPair',
    id `Ptr CUChar' } -> `CInt' #}

{#fun nvpair_value_int16
  { `NVPair',
    id `Ptr CInt' } -> `CInt' #}

{#fun nvpair_value_uint16
  { `NVPair',
    id `Ptr CUShort' } -> `CInt' #}

{#fun nvpair_value_int32
  { `NVPair',
    id `Ptr CInt' } -> `CInt' #}

{#fun nvpair_value_uint32
  { `NVPair',
    id `Ptr CUInt' } -> `CInt' #}

{#fun nvpair_value_int64
  { `NVPair',
    id `Ptr CInt' } -> `CInt' #}

{#fun nvpair_value_uint64
  { `NVPair',
    id `Ptr CULong' } -> `CInt' #}

{#fun nvpair_value_string
  { `NVPair',
    id `Ptr CString' } -> `CInt' #}

{#fun nvpair_value_nvlist
  { `NVPair',
    id `Ptr NVList' } -> `CInt' #}

{#fun nvpair_value_boolean_array
  { `NVPair',
    id `Ptr (Ptr CInt)',
    id `Ptr CUInt' } -> `CInt' #}

{#fun nvpair_value_byte_array
  { `NVPair',
    id `Ptr (Ptr CUChar)',
    id `Ptr CUInt' } -> `CInt' #}

{#fun nvpair_value_int8_array
  { `NVPair',
    id `Ptr (Ptr CInt)',
    id `Ptr CUInt' } -> `CInt' #}

{#fun nvpair_value_uint8_array
  { `NVPair',
    id `Ptr (Ptr CUChar)',
    id `Ptr CUInt' } -> `CInt' #}

{#fun nvpair_value_int16_array
  { `NVPair',
    id `Ptr (Ptr CInt)',
    id `Ptr CUInt' } -> `CInt' #}

{#fun nvpair_value_uint16_array
  { `NVPair',
    id `Ptr (Ptr CUShort)',
    id `Ptr CUInt' } -> `CInt' #}

{#fun nvpair_value_int32_array
  { `NVPair',
    id `Ptr (Ptr CInt)',
    id `Ptr CUInt' } -> `CInt' #}

{#fun nvpair_value_uint32_array
  { `NVPair',
    id `Ptr (Ptr CUInt)',
    id `Ptr CUInt' } -> `CInt' #}

{#fun nvpair_value_int64_array
  { `NVPair',
    id `Ptr (Ptr CInt)',
    id `Ptr CUInt' } -> `CInt' #}

{#fun nvpair_value_uint64_array
  { `NVPair',
    id `Ptr (Ptr CULong)',
    id `Ptr CUInt' } -> `CInt' #}

{#fun nvpair_value_string_array
  { `NVPair',
    id `Ptr (Ptr CString)',
    id `Ptr CUInt' } -> `CInt' #}

{#fun nvpair_value_nvlist_array
  { `NVPair',
    id `Ptr (Ptr NVList)',
    id `Ptr CUInt' } -> `CInt' #}

-- int nvpair_value_hrtime(nvpair_t *, hrtime_t *);
#if !defined(_KERNEL)
{#fun nvpair_value_double
  { `NVPair',
    id `Ptr CDouble' } -> `CInt' #}
#endif

-- * TODO: add "force versions" (e.g. fnvlist_alloc)?

-- * Base function imports
