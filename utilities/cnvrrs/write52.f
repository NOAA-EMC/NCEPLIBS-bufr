	SUBROUTINE WRITE52 ( iunout )

	USE	MOD_R60
	USE	MOD_MRG

	REAL*8	rwork

C*	Open the output BUFR message and begin a new BUFR subset.

	ibfdt = ( rdattim (1) * 1000000 )  + ( rdattim (2) * 10000 )  +
     +          ( rdattim (3) * 100 )  +  rdattim (4)

	CALL OPENMB ( iunout, 'USMIGRSN', ibfdt )

	CALL PKVS01 ( 'MINU', IDNINT( rdattim (5) ) )
	CALL PKVS01 ( 'SECO', IDNINT( rdattim (6) ) )
	CALL PKVS01 ( 'MTYP', 2 )
	CALL PKVS01 ( 'MSBT', 255 )
	CALL PKVS01 ( 'MSBTI', 4 )
	CALL PKVS01 ( 'MTV', 18 )

C*	Store the launch date-time

	rwork = 18.
	CALL UFBINT ( iunout, rwork, 1, 1, nlv, 'TSIG' )
	CALL UFBINT ( iunout, rdattim, 6, 1, nlv,
     +		'YEAR MNTH DAYS HOUR MINU SECO' )

C*	Store the station information.

	CALL UFBINT ( iunout, rinfo, 14, 1, nlv, STINFO )

C*	Store the cloud data.

	CALL UFBREP ( iunout, rcltp, 1, 3, nwrk, 'CLTP' )
	CALL UFBINT ( iunout, rclam, 1, 1, nwrk, 'CLAM' )
	CALL UFBINT ( iunout, rhocb, 1, 1, nwrk, 'HOCB' )

C*	Store the flight termination code.

	CALL UFBINT ( iunout, rterm, 1, 1, nlv, 'RTERM' )

C*	Store the merged level data.

	CALL DRFINI ( iunout, nmrg, 1, '(TDWPRAOB)' )
	CALL UFBSEQ ( iunout, mrg, 10, nmrg, nlv, 'TDWPRAOB' )

C*	Store the serial number and software version.

	CALL HOLD4WLC ( iunout, rserl, 'RSERL' )
	CALL HOLD4WLC ( iunout, softv, 'SOFTV' )
	
C*	Finish and store the BUFR subset.
 
	CALL WRITSB ( iunout )

	RETURN
	END
