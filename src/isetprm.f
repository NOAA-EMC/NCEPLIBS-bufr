C> @file
C> @brief Define a customized parameter value for dynamic allocation.
C>
C> @author J. Ator @date 2014-12-04

C> Set a specified parameter to a specified value for
C> use in dynamically allocating one or more internal arrays within
C> the NCEPLIBS-bufr software.
C>
C> A separate call to this
C> function must be made for each parameter that is to be set to a
C> customized value, and all such calls must be made prior to the
C> first call to subroutine openbf() from within an application
C> program, because that is when all internal arrays are dynamically
C> allocated based on the parameter values in effect at the time.
C> Otherwise, if this function is never called for a particular
C> parameter, then an internal default value for that parameter is
C> used instead.
C>
C> @param[in] CPRMNM - character*(*): Parameter to be changed from its
C> internal default value:
C> - 'MXMSGL' = Maximum length (in bytes) of a BUFR message.
C> - 'MAXSS'  = Maximum number of data values in an uncompressed BUFR
C> subset.
C> - 'MXCDV'  = Maximum number of data values that can be written into a
C> compressed BUFR subset.
C> - 'MXLCC'  = Maximum length (in bytes) of a character string that can
C> be written into a compressed BUFR subset.
C> - 'MXCSB'  = Maximum number of subsets that can be written into a
C> compressed BUFR message.
C> - 'NFILES' = Maximum number of BUFR files that can be accessed for
C> reading or writing at any one time.
C> - 'MAXTBA' = Maximum number of entries in internal BUFR Table A per
C> BUFR file.
C> - 'MAXTBB' = Maximum number of entries in internal BUFR Table B per
C> BUFR file.
C> - 'MAXTBD' = Maximum number of entries in internal BUFR Table D per
C> BUFR file.
C> - 'MAXMEM' = Maximum number of bytes that can be used to store BUFR
C> messages in internal memory.
C> - 'MAXMSG' = Maximum number of BUFR messages that can be stored in
C> internal memory.
C> - 'MXDXTS' = Maximum number of dictionary tables that can be stored
C> for use with BUFR messages in internal memory.
C> - 'MXMTBB' = Maximum number of master Table B entries.
C> - 'MXMTBD' = Maximum number of master Table D entries.
C> - 'MXMTBF' = Maximum number of master Code/Flag entries.
C> - 'MAXCD'  = Maximum number of child descriptors in a Table D
C> descriptor sequence definition.
C> - 'MAXJL'  = Maximum number of entries in the internal jump/link table.
C> - 'MXS01V' = Maximum number of default Section 0 or Section 1 values
C> that can be overwritten within an output BUFR message.
C> - 'MXBTM'  = Maximum number of bitmaps that can be stored internally
C> for a BUFR subset.
C> - 'MXBTMSE' = Maximum number of entries that can be set within a bitmap.
C> - 'MXTAMC' = Maximum number of Table A mnemonics in the internal
C> jump/link table which contain at least one Table C operator with
C> XX >= 21 in their subset definition.
C> - 'MXTCO'  = Maximum number of Table C operators with XX >= 21 in the
C> subset definition of a Table A mnemonic.
C> - 'MXNRV'  = Maximum number of 2-03 reference values in the internal
C> jump/link table.
C> - 'MXRST'  = Maximum number of long character strings that can be
C> read from a compressed subset.
C> - 'MXH4WLC' = Maximum number of long character strings that can be
C> stored internally.
C> - 'MXCNEM' = Maximum number of Table A entries that can be cached during
C> Section 3 decoding of BUFR messages.
C> @param[in] IPVAL - integer: Value to be set for CPRMNM
C> @returns  isetprm - integer:
C> -  0 = normal return
C> - -1 = Unknown CPRMNM
C>
C> @author J. Ator @date 2014-12-04
        RECURSIVE FUNCTION ISETPRM ( CPRMNM, IPVAL ) RESULT ( IRET )

        use modv_vars, only: mxmsgl, maxss, nfiles, mxdxts, maxmsg,
     .                       maxmem, maxtba, maxtbb, maxtbd, maxjl,
     .                       mxcdv, mxlcc, mxcsb, mxmtbb, mxmtbd,
     .                       mxmtbf, maxcd, mxs01v, mxbtm, mxbtmse,
     .                       mxtamc, mxtco, mxnrv, mxrst, mxh4wlc, im8b,
     .                       mxcnem

        CHARACTER*(*)   CPRMNM
        CHARACTER*128   ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C       Check for I8 integers.

        IF ( IM8B ) THEN
            IM8B = .FALSE.

            CALL X84 ( IPVAL, MY_IPVAL, 1 )
            IRET = ISETPRM ( CPRMNM, MY_IPVAL )

            IM8B = .TRUE.
            RETURN
        ENDIF

        IRET = 0
        IF ( CPRMNM .EQ. 'MAXSS' ) THEN
            MAXSS = IPVAL
        ELSE IF ( CPRMNM .EQ. 'NFILES' ) THEN
            NFILES = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXMSGL' ) THEN
            MXMSGL = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXDXTS' ) THEN
            MXDXTS = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MAXMSG' ) THEN
            MAXMSG = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MAXMEM' ) THEN
            MAXMEM = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MAXTBA' ) THEN
            MAXTBA = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MAXTBB' ) THEN
            MAXTBB = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MAXTBD' ) THEN
            MAXTBD = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MAXJL' ) THEN
            MAXJL = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXCDV' ) THEN
            MXCDV = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXLCC' ) THEN
            MXLCC = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXCSB' ) THEN
            MXCSB = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXMTBB' ) THEN
            MXMTBB = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXMTBD' ) THEN
            MXMTBD = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXMTBF' ) THEN
            MXMTBF = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MAXCD' ) THEN
            MAXCD = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXS01V' ) THEN
            MXS01V = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXBTM' ) THEN
            MXBTM = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXBTMSE' ) THEN
            MXBTMSE = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXTAMC' ) THEN
            MXTAMC = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXTCO' ) THEN
            MXTCO = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXNRV' ) THEN
            MXNRV = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXRST' ) THEN
            MXRST = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXH4WLC' ) THEN
            MXH4WLC = IPVAL
        ELSE IF ( CPRMNM .EQ. 'MXCNEM' ) THEN
            MXCNEM = IPVAL
        ELSE
            IRET = -1
            CALL ERRWRT('++++++++++++++++++WARNING+++++++++++++++++++')
            ERRSTR = 'BUFRLIB: ISETPRM - UNKNOWN INPUT PARAMETER '//
     .          CPRMNM // ' -- NO ACTION WAS TAKEN'
            CALL ERRWRT(ERRSTR)
            CALL ERRWRT('++++++++++++++++++WARNING+++++++++++++++++++')
        ENDIF

        RETURN
        END
