	MODULE MOD_R60

	  CHARACTER*(*)	STINFO
	  PARAMETER ( STINFO =
     +	    'WMOB WMOS CLATH CLONH HBMSL HEIT RATP ' //
     +	    'SIRC TTSS RASCN RFREQ PSENS TSENS RHSENS' )

	  REAL*8  :: rinfo (14), rdattim (6), rcltp (1,3),
     +		     rclam, rhocb, rterm

	  CHARACTER :: rserl*20, softv*12

	END MODULE
