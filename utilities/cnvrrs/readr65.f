	SUBROUTINE READR65 ( iunrrs )

	USE	MOD_R65

	REAL*8	PI, RTD
	PARAMETER  ( PI = 3.14159265  )
	PARAMETER  ( RTD = 180. / PI  )

	REAL*8	rwork (2,2), uwnd, vwnd


	DO WHILE ( IREADSB ( iunrrs ) .eq. 0 )

	  IF ( ( nr65 + 1 ) .gt. MXR65 ) RETURN

	  nr65 = nr65 + 1

C*	  Get the time displacement.

	  CALL GETLTDS ( iunrrs, r65 ( 1, nr65 ) )

C*	  Get the latitude and longitude displacements.

	  CALL UFBREP ( iunrrs, rwork, 2, 2, nwrk, 'CLATH CLONH' )
	  r65 ( 2, nr65 ) = rwork (1,2) - rwork (1,1)
	  r65 ( 3, nr65 ) = rwork (2,2) - rwork (2,1)

C*	  Get the wind direction and speed.

	  CALL UFBINT ( iunrrs, rwork, 2, 2, nwrk, 'UWND VWND' )
	  uwnd = rwork (1,1)
	  vwnd = rwork (2,1)
	  IF ( ( IBFMS ( uwnd ) .eq. 0 ) .and.
     +	       ( IBFMS ( vwnd ) .eq. 0 )  ) THEN
	    IF ( ( uwnd .eq. 0. ) .and. ( vwnd .eq. 0. ) ) THEN
	      r65 ( 4, nr65 ) = 0.
	    ELSE
	      r65 ( 4, nr65 ) =
     +		DATAN2 ( uwnd * (-1), vwnd * (-1) ) * RTD
	      IF ( r65 ( 4, nr65 ) .lt. 0. )
     +		r65 ( 4, nr65 ) = r65 ( 4, nr65 ) + 360.
	    ENDIF
	    r65 ( 5, nr65 ) =
     +		DSQRT ( ( uwnd ** 2 ) + ( vwnd ** 2 ) )
	  ELSE
	    r65 ( 4, nr65 ) = uwnd
	    r65 ( 5, nr65 ) = vwnd
	  ENDIF

	ENDDO

	RETURN
	END
