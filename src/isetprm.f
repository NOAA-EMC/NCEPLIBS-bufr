C> @file
C> @brief Define a customized parameter value for dynamic allocation.
C>
C> @author J. Ator @date 2014-12-04

C> This function sets a specified parameter to a specified value for
C> use in dynamically allocating one or more internal arrays within
C> the BUFRLIB software.
C>
C> <p>A separate call to this
C> function must be made for each parameter that is to be set to a
C> customized value, and all such calls must be made prior to the
C> first call to subroutine openbf() from within an application
C> program, because that is when all internal arrays are dynamically
C> allocated based on the parameter values in effect at the time.
C> Otherwise, if this function is never called for a particular
C> parameter, then an internal default value for that parameter is
C> used instead.
C>
C> @param[in] CPRMNM -- character*(*): Parameter to be changed from its
C>                      internal default value
C>                     - 'MXMSGL' = Maximum length (in bytes) of a BUFR
C>                                  message
C>                     - 'MAXSS'  = Maximum number of data values in an
C>                                  uncompressed BUFR subset
C>                     - 'MXCDV'  = Maximum number of data values that
C>                                  can be written into a compressed BUFR
C>                                  subset
C>                     - 'MXLCC'  = Maximum length (in bytes) of a
C>                                  character string that can be written
C>                                  into a compressed BUFR subset
C>                     - 'MXCSB'  = Maximum number of subsets that can be
C>                                  written into a compressed BUFR
C>                                  message
C>                     - 'NFILES' = Maximum number of BUFR files that can
C>                                  be accessed for reading or writing at
C>                                  any one time
C>                     - 'MAXTBA' = Maximum number of entries in internal
C>                                  BUFR Table A per BUFR file
C>                     - 'MAXTBB' = Maximum number of entries in internal
C>                                  BUFR Table B per BUFR file
C>                     - 'MAXTBD' = Maximum number of entries in internal
C>                                  BUFR Table D per BUFR file
C>                     - 'MAXMEM' = Maximum number of bytes that can be
C>                                  used to store BUFR messages in
C>                                  internal memory
C>                     - 'MAXMSG' = Maximum number of BUFR messages that
C>                                  can be stored in internal memory
C>                     - 'MXDXTS' = Maximum number of dictionary tables
C>                                  that can be stored for use with BUFR
C>                                  messages in internal memory
C>                     - 'MXMTBB' = Maximum number of master Table B
C>                                  entries
C>                     - 'MXMTBD' = Maximum number of master Table D
C>                                  entries
C>                     - 'MXMTBF' = Maximum number of master Code/Flag
C>                                 entries
C>                     - 'MAXCD'  = Maximum number of child descriptors
C>                                  in a Table D descriptor sequence
C>                                  definition
C>                     - 'MAXJL'  = Maximum number of entries in the
C>                                  internal jump/link table
C>                     - 'MXS01V' = Maximum number of default Section 0
C>                                  or Section 1 values that can be
C>                                  overwritten within an output BUFR
C>                                  message
C>                     - 'MXBTM'  = Maximum number of bitmaps that can be
C>                                  stored internally for a BUFR subset
C>                     - 'MXBTMSE' = Maximum number of entries that can
C>                                   be set within a bitmap
C>                     - 'MXTAMC' = Maximum number of Table A mnemonics
C>                                  in the internal jump/link table which
C>                                  contain at least one Table C operator
C>                                  with XX >= 21 in their subset definition
C>                     - 'MXTCO'  = Maximum number of Table C operators
C>                                  with XX >= 21 in the subset definition
C>                                  of a Table A mnemonic
C>                     - 'MXNRV'  = Maximum number of 2-03 reference
C>                                  values in the internal jump/link
C>                                  table
C>                     - 'MXRST'  = Maximum number of long character
C>                                  strings that can be read from a
C>                                  compressed subset
C> @param[in] IPVAL -- integer: Value to be set for CPRMNM
C> @returns  isetprm -- integer: return code
C>                      -  0 = normal return
C>                      - -1 = Unknown CPRMNM
C>
C> @author J. Ator @date 2014-12-04
        RECURSIVE FUNCTION ISETPRM ( CPRMNM, IPVAL ) RESULT ( IRET )

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
        USE MODV_IM8B

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
