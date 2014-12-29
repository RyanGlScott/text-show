#ifndef INLINE_H
#define INLINE_H

#define OPEN_PRAGMA {-#
#define CLOSE_PRAGMA #-}

#if __GLASGOW_HASKELL__ >= 702
#define INLINE_INST_FUN(F) OPEN_PRAGMA INLINE F CLOSE_PRAGMA
#else
#define INLINE_INST_FUN(F)
#endif

#endif
