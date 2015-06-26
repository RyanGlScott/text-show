#ifndef GENERIC_H
#define GENERIC_H

#if __GLASGOW_HASKELL__ >= 709 || \
   (__GLASGOW_HASKELL__ == 708 && \
    defined(__GLASGOW_HASKELL_PATCHLEVEL1__) && \
    __GLASGOW_HASKELL_PATCHLEVEL1__ == 4)
# define __LANGUAGE_DERIVE_GENERIC1__ // Workaround for https://ghc.haskell.org/trac/ghc/ticket/9563
#endif

#endif
