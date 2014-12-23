#ifndef INLINE_H
#define INLINE_H

#if __GLASGOW_HASKELL__ >= 702
#define INLINE(F) {-# INLINE F #-}
#else
#define INLINE(F)
#endif

#endif
