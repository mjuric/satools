c ===========================================================
c xephem.h  - flags and data for xephem interface with fitobs
c patch 1.6.1: A.Milani, May 2, 1998, integration level 1
c ===========================================================
c integration level 0=none 
      INTEGER ixeph
c warning: so far, if the input fiotbs.xephem is missing, it is set
c to zero in finobs.f - this might not be the right way to do it
      COMMON/xepdat/ixeph
