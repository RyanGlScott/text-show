#ifndef OVERLAP_H
#define OVERLAP_H

#if __GLASGOW_HASKELL__ >= 710
# define __LANGUAGE_OVERLAPPING_INSTANCES__
# define __OVERLAPPABLE__ {-# OVERLAPPABLE #-}
# define __OVERLAPPING__  {-# OVERLAPPING #-}
# define __OVERLAPS__     {-# OVERLAPS #-}
#else
# define __LANGUAGE_OVERLAPPING_INSTANCES__ {-# LANGUAGE OverlappingInstances #-}
# define __OVERLAPPABLE__
# define __OVERLAPPING__
# define __OVERLAPS__
#endif

#endif
