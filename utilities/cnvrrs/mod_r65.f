	MODULE MOD_R65
 
C*	  The r65 array will hold values read or derived from as many
C*	  as MXR65 different subsets of RRS data encoded according to
C*	  Table D descriptor 3-09-065.  The values stored for each
C*	  subset will be (in order):
C*	    1. LTDS
C*	    2. LATDH
C*	    3. LONDH
C*	    4. WDIR
C*	    5. WSPD

	  PARAMETER ( MXR65 = 12000 )

	  REAL*8  :: r65 ( 5, MXR65 )

	  INTEGER :: nr65 = 0

	END MODULE
