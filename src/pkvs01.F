C> @file
C> @brief Specify a value to be included in Section 0 or Section 1 when
C> writing BUFR messages.

C> This subroutine is used to specify a value to be written into a
C> specified location within Section 0 or Section 1 of all BUFR messages
C> output by future calls to subroutines writsb(), writsa(), writcp(),
C> copybf(), copymg() or cpymem().
C>
C> <p>This subroutine is similar to subroutine pkbs1(), except that
C> pkbs1() operates on a single BUFR message passed in via a memory
C> array.  Alternatively, whenever this subroutine is called, the
C> specified IVAL will be written into all BUFR messages output by all
C> future calls to subroutines writsb(), writsa(), writcp(), copybf(),
C> copymg() or cpymem(), for all Fortran logical units that are open for
C> output within the application program, unless a subsequent call is
C> made to this subroutine with the same value of S01MNEM in order to
C> reset the corresponding IVAL again.  Otherwise, if this subroutine
C> is never called for a particular value of S01MNEM, then a default
C> value is used for the corresponding IVAL, as set within subroutine
C> msgini(), cmsgini() or dxmini().
C>
C> @authors J. Ator
C> @date 2005-11-29
C>
C> @param[in]     IVAL    - integer: Value to be stored
C> @param[in]  S01MNEM    - character*(*): Location where IVAL is to
C>                          be stored within Section 0 or Section 1 of
C>                          all future output BUFR messages
C>                          - 'BEN'   = BUFR edition number
C>                          - 'BMT'   = BUFR master table
C>                          - 'OGCE'  = Originating center
C>                          - 'GSES'  = Originating subcenter
C>                          - 'USN'   = Update sequence number
C>                          - 'MTYP'  = Data category
C>                          - 'MSBTI' = Data subcategory (international)
C>                          - 'MSBT'  = Data subcategory (local)
C>                          - 'MTV'   = Version number of master table
C>                          - 'MTVL'  = Version number of local tables
C>                          - 'YCEN'  = Year of century (1-100)
C>                          - 'CENT'  = Century (e.g., 20 for years 1901-2000,
C>                                      21 for years 2001-2100)
C>                          - 'YEAR'  = Year (4-digit)
C>                          - 'MNTH'  = Month
C>                          - 'DAYS'  = Day
C>                          - 'HOUR'  = Hour
C>                          - 'MINU'  = Minute
C>                          - 'SECO'  = Second
C>
C> @remarks
C> - A separate call to this subroutine must be made for each value of
C>   S01MNEM that is to be set within Section 0 or Section 1 of
C>   all future output BUFR messages.
C> - A call to this subroutine with S01MNEM = 'BEN' and IVAL = 4 will
C>   force all future output BUFR messages to be encoded using BUFR
C>   edition 4; otherwise, messages will be encoded using BUFR edition 3
C>   by default.
C> - Values corresponding to S01MNEM = 'YCEN' or 'CENT' can only be stored
C>   within BUFR messages encoded using BUFR edition 3.
C> - Values corresponding to S01MNEM = 'YEAR', 'SECO' or 'MSBTI' can only
C>   be stored within BUFR messages encoded using BUFR edition 4.
C>
C> <b>Program history log:</b>
C> - 2005-11-29  J. Ator    -- Original author
C> - 2006-04-14  D. Keyser  -- Updated docblock
C> - 2015-03-03  J. Ator    -- Use module MODA_S01CM
C>
      SUBROUTINE PKVS01(S01MNEM,IVAL)

      USE MODA_S01CM

      INCLUDE 'bufrlib.inc'

      CHARACTER*(*) S01MNEM

      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

#ifdef DYNAMIC_ALLOCATION

C     CONFIRM THAT THE ARRAYS NEEDED BY THIS SUBROUTINE HAVE ALREADY
C     BEEN ALLOCATED (AND IF NOT, GO AHEAD AND ALLOCATE THEM NOW), SINCE
C     IT'S POSSIBLE FOR THIS SUBROUTINE TO BE CALLED BEFORE THE FIRST
C     CALL TO SUBROUTINE OPENBF.

      IF ( ( .NOT. ALLOCATED(CMNEM) ) .OR.
     .     ( .NOT. ALLOCATED(IVMNEM) ) )  THEN
        CALL OPENBF(0,'FIRST',0)
      ENDIF
#endif

C     IF AN IVAL HAS ALREADY BEEN ASSIGNED FOR THIS PARTICULAR S01MNEM,
C     THEN OVERWRITE THAT ENTRY IN COMMON /S01CM/ USING THE NEW IVAL.

      IF(NS01V.GT.0) THEN
        DO I=1,NS01V
          IF(S01MNEM.EQ.CMNEM(I)) THEN
            IVMNEM(I) = IVAL
            RETURN
          ENDIF
        ENDDO
      ENDIF

C     OTHERWISE, USE THE NEXT AVAILABLE UNUSED ENTRY IN COMMON /S01CM/.

      IF(NS01V.GE.MXS01V) GOTO 900

      NS01V = NS01V + 1
      CMNEM(NS01V) = S01MNEM
      IVMNEM(NS01V) = IVAL

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: PKVS01 - CANNOT OVERWRITE MORE THAN '//
     . '",I2," DIFFERENT LOCATIONS WITHIN SECTION 0 OR SECTION 1")')
     . MXS01V
      CALL BORT(BORT_STR)
      END
