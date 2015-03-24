module ZFS.Primitive.AVL where
import Foreign
import Foreign.C

{#context lib = "zfs" #}

#define HAVE_IOCTL_IN_SYS_IOCTL_H
#include <sys/avl.h>

{#pointer *avl_tree_t as AVLTree newtype #}
{#pointer *avl_node_t as AVLNode newtype #}
-- {#pointer avl_index_t as AVLIndex newtype #}
newtype AVLIndex = AVLIndex { fromAVLIndex :: CULong }

avlBefore = {#const AVL_BEFORE #}
avlAfter = {#const AVL_AFTER #}

{#fun avl_create as ^
  { `AVLTree',
    id `FunPtr (Ptr () -> Ptr () -> IO CInt)',
    `CULong',
    `CULong' } -> `()' #}

{#fun avl_find as ^
  { `AVLTree',
    `Ptr ()',
    castPtr `Ptr AVLIndex' } -> `()' #}

{#fun avl_insert as ^
  { `AVLTree',
    `Ptr ()',
    fromAVLIndex `AVLIndex' } -> `()' #}

{#fun avl_insert_here as ^
  { `AVLTree',
    `Ptr ()',
    `Ptr ()',
    `CInt' } -> `()' #}

{#fun avl_first as ^
  { `AVLTree' } -> `()' #}

{#fun avl_last as ^
  { `AVLTree' } -> `()' #}

-- avlNext tree node = avlWalk tree node avlAfter
-- avlPrev tree node = avlWalk tree node avlBefore

{#fun avl_nearest as ^
  { `AVLTree',
    fromAVLIndex `AVLIndex',
    `CInt' } -> `()' #}

{#fun avl_add as ^
  { `AVLTree',
    `Ptr ()' } -> `()' #}

{#fun avl_remove as ^
  { `AVLTree',
    `Ptr ()' } -> `()' #}

{#fun avl_update as ^
  { `AVLTree',
    `Ptr ()' } -> `()' #}

{#fun avl_update_lt as ^
  { `AVLTree',
    `Ptr ()' } -> `()' #}

{#fun avl_update_gt as ^
  { `AVLTree',
    `Ptr ()' } -> `()' #}

{#fun avl_numnodes as ^
  { `AVLTree' } -> `CULong' #}

{#fun avl_is_empty as ^
  { `AVLTree' } -> `Bool' toBool #}

{#fun avl_destroy_nodes as ^
  { `AVLTree',
    id `Ptr (Ptr ())' } -> `Ptr ()' #}

{#fun avl_destroy as ^
  { `AVLTree' } -> `()' #}

