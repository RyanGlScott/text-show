{-
INLINE pragmas for instance declarations don't appear to work on GHC 7.0.4,
so we use dirty CPP hackery to conditionally INLINE them.
-}

#ifndef INLINE_H
#define INLINE_H

#if __GLASGOW_HASKELL__ >= 702
#define INLINE(F) {-# INLINE F #-}
#else
#define INLINE(F)
#endif

#endif
