	SUBROUTINE READR64 ( iunrrs )

	USE	MOD_R64

	REAL*8	rwork (2,2)


	DO WHILE ( IREADSB ( iunrrs ) .eq. 0 )

	  IF ( ( nr64 + 1 ) .gt. MXR64 ) RETURN

	  nr64 = nr64 + 1

C*	  Get the time displacement.

	  CALL GETLTDS ( iunrrs, r64 ( 1, nr64 ) )

C*	  Get the height and dew point temperature.

	  CALL UFBINT ( iunrrs, rwork, 2, 2, nwrk, 'GPH10 TMDP' )
	  r64 ( 3, nr64 ) = rwork (1,1)
	  r64 ( 5, nr64 ) = rwork (2,1)

C*	  Get the "best" (i.e. smoothed/checked) pressure value.

	  CALL UFBREP ( iunrrs, rwork, 2, 2, nwrk, 'FLPC PRLC' )
	  IF (  ( nwrk .eq. 2 ) .and. 
     +		( rwork (1,2) .ge. rwork (1,1) ) .and.
     +		( IBFMS ( rwork (1,2) ) .eq. 0 ) .and.
     +		( IBFMS ( rwork (2,2) ) .eq. 0 )  ) THEN
	    jj = 2
	  ELSE
	    jj = 1
	  ENDIF
	  r64 ( 2, nr64 ) = rwork ( 2, jj )

C*	  Get the "best" (i.e. solar/infrared corrected) temperature
C*	  value.

	  CALL UFBREP ( iunrrs, rwork, 2, 2, nwrk, 'SIRC TMDB' )
	  IF (  ( nwrk .eq. 2 ) .and. 
     +		( rwork (1,2) .ge. rwork (1,1) ) .and.
     +		( IBFMS ( rwork (1,2) ) .eq. 0 ) .and.
     +		( IBFMS ( rwork (2,2) ) .eq. 0 )  ) THEN
	    jj = 2
	  ELSE
	    jj = 1
	  ENDIF
	  r64 ( 4, nr64 ) = rwork ( 2, jj )

	ENDDO

	RETURN
	END
