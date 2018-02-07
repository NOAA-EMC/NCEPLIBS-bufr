	MODULE MOD_R66

C*	  The r66 array will hold values read or derived from as many
C*	  as MXR66 different subsets of RRS data encoded according to
C*	  Table D descriptor 3-09-066.  The values stored for each
C*	  subset will be (in order):
C*	    1. LTDS
C*	    2. VSIGX
C*	    3. PRLC
C*	    4. GPH10
C*	    5. TMDB
C*	    6. TMDP
C*	    7. WDIR
C*	    8. WSPD

	  PARAMETER ( MXR66 = 3000 )

	  REAL*8  :: r66 ( 8, MXR66 )

	  INTEGER :: nr66 = 0

	END MODULE
