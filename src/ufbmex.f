C> @file
C> @brief Connect a new system file to the BUFRLIB software, and read
C> the entire file contents into internal arrays.

C> This subroutine connects a new system file to the BUFRLIB software
C> for input operations, then reads the entire file contents into
C> internal arrays so that any of the individual BUFR messages can
C> later be accessed from memory, instead of having to read them one
C> at a time sequentially from the system file.
C>
C> <p>This subroutine is similar to subroutine ufbmem(), except that
C> instead of a file status it returns an array of message types that
C> were read in.  Furthermore, this subroutine doesn't process any
C> embedded DX BUFR tables contained within the system file; instead,
C> it provides an additional call argument LUNDX to allow
C> for specification of the necessary DX BUFR table information
C> associated with the messages in the file.
C>
C> @author J. Woollen
C> @date 2012-01-26
C>
C> @param[in] LUNIT   -- integer: Fortran logical unit number for BUFR
C>                       file
C> @param[in] LUNDX   -- integer: Fortran logical unit number
C>                       containing DX BUFR table information
C>                       associated with BUFR messages in LUNIT
C> @param[in] INEW    -- integer: Processing option
C>                       - 0 = Initialize the internal arrays, then
C>                             read all BUFR messages from LUNIT into
C>                             internal arrays
C>                       - Otherwise, read all BUFR messages from LUNIT
C>                         and append them to the existing messages
C>                         within the internal arrays
C> @param[out] IRET   -- integer: Number of BUFR messages that were
C>                       read from LUNIT and stored into internal arrays
C> @param[out] MESG   -- integer(*): Types of BUFR messages that were
C>                       read from LUNIT and stored into internal arrays
C>
C> <p>Logical unit numbers LUNIT and LUNDX must already be associated
C> with actual filenames on the local system, typically via a Fortran
C> "OPEN" statement.
C>      
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2012-01-26 | J. Woollen | Original author |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2015-09-24 | D. Stokes  | Fix missing declaration of COMMON /QUIET/ |
C> | 2022-10-04 | J. Ator    | Added 8-byte wrapper |

      RECURSIVE SUBROUTINE UFBMEX(LUNIT,LUNDX,INEW,IRET,MESG)

      USE MODV_IM8B
      USE MODV_MAXMEM
      USE MODV_MAXMSG

      USE MODA_MGWA
      USE MODA_MSGMEM

      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR,ERRSTR

      INTEGER       MESG(*), IRET(*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(LUNDX,MY_LUNDX,1)
         CALL X84(INEW,MY_INEW,1)
         IF (MY_INEW.EQ.0) THEN
            NMESG = 0
         ELSE
            NMESG = MSGP(0)
            CALL X84(MESG,MESG,NMESG)
         ENDIF
         CALL UFBMEX(MY_LUNIT,MY_LUNDX,MY_INEW,IRET,MESG)
         CALL X48(MESG,MESG,NMESG+IRET(1))
         CALL X48(IRET,IRET,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  TRY TO OPEN BUFR FILE AND SET TO INITIALIZE OR CONCATENATE
C  ----------------------------------------------------------

      CALL OPENBF(LUNIT,'IN',LUNDX)

      IF(INEW.EQ.0) THEN
         MSGP(0) = 0
         MUNIT = 0
         MLAST = 0
         NDXTS = 0
         LDXTS = 0
         NDXM = 0
         LDXM = 0
      ENDIF

      NMSG = MSGP(0)
      IRET(1) = 0
      IFLG = 0
      ITIM = 0

C  SET SOME FLAGS SO THAT SUBSEQUENT CALLS TO THE MESSAGE READING
C  ROUTINES WILL KNOW THERE IS A BUFR TABLE IN SCOPE.

      NDXTS = 1
      LDXTS = 1
      IPMSGS(1) = 1

C  TRANSFER MESSAGES FROM FILE TO MEMORY - SET MESSAGE POINTERS
C  ------------------------------------------------------------

1     CALL RDMSGW(LUNIT,MGWA,IER)
      IF(IER.EQ.-1) GOTO 100
      IF(IER.EQ.-2) GOTO 900

      NMSG = NMSG+1
      MESG(NMSG) = IUPBS01(MGWA,'MTYP')
      IF(NMSG      .GT.MAXMSG) IFLG = 1
      LMEM = NMWRD(MGWA)
      IF(LMEM+MLAST.GT.MAXMEM) IFLG = 2

      IF(IFLG.EQ.0) THEN
         IRET(1) = IRET(1)+1
         DO I=1,LMEM
            MSGS(MLAST+I) = MGWA(I)
         ENDDO
         MSGP(0)    = NMSG
         MSGP(NMSG) = MLAST+1
      ELSE
         IF(ITIM.EQ.0) THEN
            MLAST0 = MLAST
            ITIM=1
         ENDIF
      ENDIF
      MLAST = MLAST+LMEM
      GOTO 1

C  EXITS
C  -----

100   IF(IFLG.EQ.1) THEN

C  EMERGENCY ROOM TREATMENT FOR MAXMSG ARRAY OVERFLOW
C  --------------------------------------------------

      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,A,I8,A)' )
     . 'BUFRLIB: UFBMEX - THE NO. OF MESSAGES REQUIRED TO STORE ',
     . 'ALL MESSAGES INTERNALLY EXCEEDS MAXIMUM (', MAXMSG, 
     . ') - INCOMPLETE READ'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBMEX STORED ', MSGP(0), ' MESSAGES OUT OF ', NMSG, '<<<'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBMEX STORED ', MLAST0, ' BYTES OUT OF ', MLAST, '<<<'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF
      MLAST=MLAST0
      ENDIF

      IF(IFLG.EQ.2) THEN

C  EMERGENCY ROOM TREATMENT FOR MAXMEM ARRAY OVERFLOW
C  --------------------------------------------------

      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,A,I8,A)' )
     . 'BUFRLIB: UFBMEX - THE NO. OF BYTES REQUIRED TO STORE ',
     . 'ALL MESSAGES INTERNALLY EXCEEDS MAXIMUM (', MAXMEM, 
     . ') - INCOMPLETE READ'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBMEX STORED ', MLAST0, ' BYTES OUT OF ', MLAST, '<<<'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBMEX STORED ', MSGP(0), ' MESSAGES OUT OF ', NMSG, '<<<'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF
      MLAST=MLAST0
      ENDIF

      IF(IRET(1).EQ.0) THEN
         CALL CLOSBF(LUNIT)
      ELSE
         IF(MUNIT.NE.0) CALL CLOSBF(LUNIT)
         IF(MUNIT.EQ.0) MUNIT = LUNIT
      ENDIF

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: UFBMEX - ERROR READING MESSAGE '//
     . 'NUMBER",I5," INTO MEMORY FROM UNIT",I3)') NMSG+1,LUNIT
      CALL BORT(BORT_STR)
      END
