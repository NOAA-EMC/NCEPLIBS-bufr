	SUBROUTINE READR66 ( iunrrs )

	USE	MOD_R66

	REAL*8	rwork (7), rltds, LVS2VSX


	DO WHILE ( IREADSB ( iunrrs ) .eq. 0 )

C*	  Get the level data.

	  CALL UFBINT ( iunrrs, rwork, 7, 1, nwrk,
     +		'LEVSIG PRLC GPH10 TMDB TMDP WDIR WSPD' )

C*	  Get the time displacement.

	  CALL GETLTDS ( iunrrs, rltds )

	  IF (  ( nr66 .gt. 0 ) .and.
     +	      ( rltds .eq. r66 ( 1, nr66 ) )  )  THEN

C*	    This is a duplicate of the previous level, but with a
C*	    different LEVSIG code table value.  Since LEVSIG is being
C*	    converted to a VSIGX flag table value, add the converted
C*	    value for this level to the VSIGX for the previous level.

	    r66 ( 2, nr66 ) = r66 ( 2, nr66 ) + LVS2VSX ( rwork (1) )

	  ELSE

C*	    Store the new level.

	    IF ( ( nr66 + 1 ) .gt. MXR66 ) RETURN
	    nr66 = nr66 + 1

	    r66 ( 1, nr66 ) = rltds
	    r66 ( 2, nr66 ) = LVS2VSX ( rwork (1) )
	    DO ii = 2, 7
	      r66 ( ii + 1, nr66 ) = rwork (ii)
	    ENDDO

	  ENDIF

	ENDDO

	RETURN
	END
