	MODULE MOD_R64

C*	  The r64 array will hold values read or derived from as many
C*	  as MXR64 different subsets of RRS data encoded according to
C*	  Table D descriptor 3-09-064.  The values stored for each
C*	  subset will be (in order):
C*	    1. LTDS
C*	    2. PRLC
C*	    3. GPH10
C*	    4. TMDB
C*	    5. TMDP

	  PARAMETER ( MXR64 = 12000 )

	  REAL*8  :: r64 ( 5, MXR64 )

	  INTEGER :: nr64 = 0

	END MODULE
