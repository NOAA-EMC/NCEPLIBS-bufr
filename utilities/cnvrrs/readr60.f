	SUBROUTINE READR60 ( iunrrs )

	USE	MOD_R60

	REAL*8	rwork (7,7)

	IF ( IREADSB ( iunrrs ) .eq. 0 ) THEN

C*	  Get the station information.  Note that HEIT is the height
C*	  of the release point above mean sea level.

	  CALL UFBINT ( iunrrs, rinfo, 14, 1, nwrk, STINFO )

C*	  Get the launch date-time.  This information is in the 4th
C*	  replication of the below mnemonic string, corresponding
C*	  to a DATSIG value of 3.

	  CALL UFBREP ( iunrrs, rwork, 7, 7, nwrk,
     +		'DATSIG YEAR MNTH DAYS HOUR MINU SECO' )
	  DO ii = 1, 6
	    rdattim (ii) = rwork (ii+1,4)
	  ENDDO

C*	  Get the cloud data.

	  CALL UFBREP ( iunrrs, rcltp, 1, 3, nwrk, 'CLTP' )
	  CALL UFBINT ( iunrrs, rclam, 1, 1, nwrk, 'CLAM' )
	  CALL UFBINT ( iunrrs, rhocb, 1, 1, nwrk, 'HOCB' )

C*	  Get the flight termination code.  This information is in
C*	  the 2nd replication of the below mnemonic string,
C*	  corresponding to a LEVSIG value of 23.

	  CALL UFBREP ( iunrrs, rwork, 7, 7, nwrk, 'LEVSIG RTERM' )
	  rterm = rwork(2,2)

C*	  Get the serial number and software version.

	  CALL READLC ( iunrrs, rserl, 'RSERL' )
	  CALL READLC ( iunrrs, softv, 'SOFTV' )

	ENDIF

	RETURN
	END
