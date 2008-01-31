* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 10, 1996
* ---------------------------------------------------------------------
* Max number of namelist entries (main namelist)
      INTEGER nnex
      PARAMETER (nnex=100)
* Max number of namelist entries (simplified file-header namelist)
      INTEGER nfnex
      PARAMETER (nfnex=99)
* Max number of entries in the list of keywords
      INTEGER nklsx
      PARAMETER (nklsx=200)
* Max number of string to integer translations
      INTEGER ns2itx
      PARAMETER (ns2itx=200)

* Length of the "key" field
      INTEGER lckx
      PARAMETER (lckx=30)
* Length of the "value" field
      INTEGER lcvx
      PARAMETER (lcvx=150)
* Length of the "filename" field
      INTEGER lcfx
      PARAMETER (lcfx=80)
