	MODULE MOD_MRG

C*	  The mrg array will hold data values derived from the merging
C*	  of all RRS subsets in order to create a complete sounding.
C*	  The values stored for each sounding level will be (in order):
C*	    1. LTDS
C*	    2. VSIGX
C*	    3. PRLC
C*	    4. GPH10
C*	    5. LATDH
C*	    6. LONDH
C*	    7. TMDB
C*	    8. TMDP
C*	    9. WDIR
C*	   10. WSPD

	  PARAMETER ( MXMRG = 14000 )

	  REAL*8  :: mrg ( 10, MXMRG )

	  INTEGER :: nmrg = 0

	END MODULE
