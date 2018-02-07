	REAL*8 FUNCTION LVS2VSX ( rlvsig )

C*	Returns VSIGX (Table B 0-08-042) flag table value corresponding
C*	to input LEVSIG (Table B 0-08-040) code table value.

	REAL*8	PKFTBV, rlvsig


	SELECT CASE ( IDINT ( rlvsig ) )
	  CASE (20)
	    ibit = 1
	  CASE (14)
	    ibit = 2
	  CASE (24)
	    ibit = 3
	  CASE (29)
	    ibit = 4
	  CASE (21)
	    ibit = 5
	  CASE (18)
	    ibit = 6
	  CASE (28)
	    ibit = 7
	  CASE (7)
	    ibit = 8
	  CASE (12)
	    ibit = 9
	  CASE (6)
	    ibit = 10 
	  CASE (11)
	    ibit = 11
	  CASE (43)
	    ibit = 12 
	  CASE (44)
	    ibit = 13
	  CASE (32)
	    ibit = 14 
	  CASE (30)
	    ibit = 15 
	  CASE DEFAULT
	    ibit = 0
	END SELECT

	IF ( ibit .eq. 0 ) THEN
	  LVS2VSX = 0.
	ELSE
	  LVS2VSX = PKFTBV ( 18, ibit )
	ENDIF

	RETURN
	END
