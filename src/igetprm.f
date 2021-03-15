C> @file
C> @brief Get the current value of a parameter

C> This function returns the current value of a parameter used
C> for allocating one or more internal arrays within the
C> BUFRLIB software.
C>
C> @author J. Ator
C> @date 2014-12-04
C>
C> @param[in] CPRMNM - character*(*): Parameter
C>                    - 'MXMSGL' = Maximum length (in bytes) of a BUFR
C>                                 message
C>                    - 'MAXSS'  = Maximum number of data values in an
C>                                 uncompressed BUFR subset
C>                    - 'MXCDV'  = Maximum number of data values that
C>                                 can be written into a compressed BUFR
C>                                 subset
C>                    - 'MXLCC'  = Maximum length (in bytes) of a
C>                                 character string that can be written
C>                                 into a compressed BUFR subset
C>                    - 'MXCSB'  = Maximum number of subsets that can be
C>                                 written into a compressed BUFR
C>                                 message
C>                    - 'NFILES' = Maximum number of BUFR files that can
C>                                 be accessed for reading or writing at
C>                                 any one time
C>                    - 'MAXTBA' = Maximum number of entries in internal
C>                                 BUFR Table A per BUFR file
C>                    - 'MAXTBB' = Maximum number of entries in internal
C>                                 BUFR Table B per BUFR file
C>                    - 'MAXTBD' = Maximum number of entries in internal
C>                                 BUFR Table D per BUFR file
C>                    - 'MAXMEM' = Maximum number of bytes that can be
C>                                 used to store BUFR messages in
C>                                 internal memory
C>                    - 'MAXMSG' = Maximum number of BUFR messages that
C>                                 can be stored in internal memory
C>                    - 'MXDXTS' = Maximum number of dictionary tables
C>                                 that can be stored for use with BUFR
C>                                 messages in internal memory
C>                    - 'MXMTBB' = Maximum number of master Table B
C>                                 entries
C>                    - 'MXMTBD' = Maximum number of master Table D
C>                                 entries
C>                    - 'MXMTBF' = Maximum number of master Code/Flag
C>                                 entries
C>                    - 'MAXCD'  = Maximum number of child descriptors
C>                                 in a Table D descriptor sequence
C>                                 definition
C>                    - 'MAXJL'  = Maximum number of entries in the
C>                                 internal jump/link table
C>                    - 'MXS01V' = Maximum number of default Section 0
C>                                 or Section 1 values that can be
C>                                 overwritten within an output BUFR
C>                                 message
C>                    - 'MXBTM'  = Maximum number of bitmaps that can be
C>                                 stored internally for a BUFR subset
C>                    - 'MXBTMSE' = Maximum number of entries that can be
C>                                  set within a bitmap
C>                    - 'MXTAMC' = Maximum number of Table A mnemonics
C>                                 in the internal jump/link table which
C>                                 contain at least one Table C operator
C>                                 with XX >= 21 in their subset definition
C>                    - 'MXTCO'  = Maximum number of Table C operators
C>                                 with XX >= 21) in the subset definition
C>                                 of a Table A mnemonic
C>                    - 'MXNRV'  = Maximum number of 2-03 reference
C>                                 values in the internal jump/link
C>                                 table
C>                    - 'MXRST'  = Maximum number of long character
C>                                 strings that can be read from a
C>                                 compressed subset
C>
C> @returns igetprm  - integer: Value of CPRMNM
C>                       - -1 = Unknown CPRMNM
C>
C> <b>Program history log:</b>
C> - 2014-12-04  J. Ator    -- Original author
C>
	INTEGER FUNCTION IGETPRM ( CPRMNM )

	USE MODV_MAXSS
	USE MODV_NFILES
	USE MODV_MXMSGL
	USE MODV_MXDXTS
	USE MODV_MAXMSG
	USE MODV_MAXMEM
	USE MODV_MAXTBA
	USE MODV_MAXTBB
	USE MODV_MAXTBD
	USE MODV_MAXJL
	USE MODV_MXCDV
	USE MODV_MXLCC
	USE MODV_MXCSB
	USE MODV_MXMTBB
	USE MODV_MXMTBD
	USE MODV_MXMTBF
	USE MODV_MAXCD
	USE MODV_MXS01V
	USE MODV_MXBTM
	USE MODV_MXBTMSE
	USE MODV_MXTAMC
	USE MODV_MXTCO
	USE MODV_MXNRV
	USE MODV_MXRST

	INCLUDE	'bufrlib.inc'

	CHARACTER*(*)	CPRMNM
	CHARACTER*64	ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	IF ( CPRMNM .EQ. 'MAXSS' ) THEN
	    IGETPRM = MAXSS
	ELSE IF ( CPRMNM .EQ. 'NFILES' ) THEN
	    IGETPRM = NFILES
	ELSE IF ( CPRMNM .EQ. 'MXMSGL' ) THEN
	    IGETPRM = MXMSGL
	ELSE IF ( CPRMNM .EQ. 'MXDXTS' ) THEN
	    IGETPRM = MXDXTS
	ELSE IF ( CPRMNM .EQ. 'MAXMSG' ) THEN
	    IGETPRM = MAXMSG
	ELSE IF ( CPRMNM .EQ. 'MAXMEM' ) THEN
	    IGETPRM = MAXMEM
	ELSE IF ( CPRMNM .EQ. 'MAXTBA' ) THEN
	    IGETPRM = MAXTBA
	ELSE IF ( CPRMNM .EQ. 'MAXTBB' ) THEN
	    IGETPRM = MAXTBB
	ELSE IF ( CPRMNM .EQ. 'MAXTBD' ) THEN
	    IGETPRM = MAXTBD
	ELSE IF ( CPRMNM .EQ. 'MAXJL' ) THEN
	    IGETPRM = MAXJL
	ELSE IF ( CPRMNM .EQ. 'MXCDV' ) THEN
	    IGETPRM = MXCDV
	ELSE IF ( CPRMNM .EQ. 'MXLCC' ) THEN
	    IGETPRM = MXLCC
	ELSE IF ( CPRMNM .EQ. 'MXCSB' ) THEN
	    IGETPRM = MXCSB
	ELSE IF ( CPRMNM .EQ. 'MXMTBB' ) THEN
	    IGETPRM = MXMTBB
	ELSE IF ( CPRMNM .EQ. 'MXMTBD' ) THEN
	    IGETPRM = MXMTBD
	ELSE IF ( CPRMNM .EQ. 'MXMTBF' ) THEN
	    IGETPRM = MXMTBF
	ELSE IF ( CPRMNM .EQ. 'MAXCD' ) THEN
	    IGETPRM = MAXCD
	ELSE IF ( CPRMNM .EQ. 'MXS01V' ) THEN
	    IGETPRM = MXS01V
	ELSE IF ( CPRMNM .EQ. 'MXBTM' ) THEN
	    IGETPRM = MXBTM
	ELSE IF ( CPRMNM .EQ. 'MXBTMSE' ) THEN
	    IGETPRM = MXBTMSE
	ELSE IF ( CPRMNM .EQ. 'MXTAMC' ) THEN
	    IGETPRM = MXTAMC
	ELSE IF ( CPRMNM .EQ. 'MXTCO' ) THEN
	    IGETPRM = MXTCO
	ELSE IF ( CPRMNM .EQ. 'MXNRV' ) THEN
	    IGETPRM = MXNRV
	ELSE IF ( CPRMNM .EQ. 'MXRST' ) THEN
	    IGETPRM = MXRST
	ELSE
	    IGETPRM = -1
	    CALL ERRWRT('++++++++++++++++++WARNING+++++++++++++++++++')
	    ERRSTR = 'BUFRLIB: IGETPRM - UNKNOWN INPUT PARAMETER '//
     .		CPRMNM
	    CALL ERRWRT(ERRSTR)
	    CALL ERRWRT('++++++++++++++++++WARNING+++++++++++++++++++')
	ENDIF

	RETURN
	END
