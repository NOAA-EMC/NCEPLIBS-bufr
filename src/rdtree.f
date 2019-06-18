      SUBROUTINE RDTREE(LUN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RDTREE
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE UNPACKS THE NEXT SUBSET FROM THE INTERNAL
C   UNCOMPRESSED MESSAGE BUFFER (ARRAY MBAY IN MODULE BITBUF) AND
C   STORES THE UNPACKED SUBSET WITHIN THE INTERNAL ARRAY VAL(*,LUN)
C   IN MODULE USRINT.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-10-27  J. WOOLLEN -- MODIFIED TO CORRECT PROBLEMS CAUSED BY IN-
C                           LINING CODE WITH FPP DIRECTIVES
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           10,000 TO 20,000 BYTES
C 2003-11-04  J. WOOLLEN -- FIXED A BUG WHICH COULD ONLY OCCUR WHEN
C                           THE LAST ELEMENT IN A SUBSET IS A CHARACTER
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED DOCUMENTATION (INCLUDING
C                           HISTORY)
C 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           20,000 TO 50,000 BYTES
C 2007-01-19  J. ATOR    -- PREVENT OVERFLOW OF CVAL FOR STRINGS LONGER
C                           THAN 8 CHARACTERS
C 2012-03-02  J. ATOR    -- USE FUNCTION UPS
C 2012-06-04  J. ATOR    -- SET DECODED REAL*8 VALUE TO "MISSING" WHEN
C                           CORRESPONDING CHARACTER FIELD HAS ALL BITS
C                           SET TO 1
C 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C
C USAGE:    CALL RDTREE (LUN)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C REMARKS:
C    THIS ROUTINE CALLS:        RCSTPL   ICBFMS   UPBB     UPC
C                               UPS
C    THIS ROUTINE IS CALLED BY: READSB
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      USE MODA_USRINT
      USE MODA_USRBIT
      USE MODA_IVAL
      USE MODA_BITBUF
      USE MODA_TABLES

      INCLUDE 'bufrlib.prm'

      CHARACTER*8  CVAL
      EQUIVALENCE  (CVAL,RVAL)
      REAL*8       RVAL,UPS

C-----------------------------------------------------------------------
C     Statement function to compute BUFR "missing value" for field
C     of length IBT(NODE)) bits (all bits "on"):

      MPS(NODE) = 2**(IBT(NODE))-1
C-----------------------------------------------------------------------

C  CYCLE THROUGH A SUBSET SETTING UP THE TEMPLATE
C  ----------------------------------------------

      MBIT(1) = IBIT
      NBIT(1) = 0
      CALL RCSTPL(LUN)

C  UNPACK A SUBSET INTO THE USER ARRAY IVAL
C  ----------------------------------------

      DO N=1,NVAL(LUN)
      CALL UPBB(IVAL(N),NBIT(N),MBIT(N),MBAY(1,LUN))
      ENDDO

C  LOOP THROUGH EACH ELEMENT OF THE SUBSET, CONVERTING THE UNPACKED
C  VALUES TO THE PROPER TYPES
C  ----------------------------------------------------------------

      DO N=1,NVAL(LUN)
      NODE = INV(N,LUN)
      IF(ITP(NODE).EQ.1) THEN

C	 The unpacked value is a delayed descriptor replication factor.

         VAL(N,LUN) = IVAL(N)
      ELSEIF(ITP(NODE).EQ.2) THEN

C	 The unpacked value is a real.

         IF (IVAL(N).LT.MPS(NODE)) THEN
            VAL(N,LUN) = UPS(IVAL(N),NODE)
         ELSE
            VAL(N,LUN) = BMISS
         ENDIF
      ELSEIF(ITP(NODE).EQ.3) THEN

C        The value is a character string, so unpack it using an
C        equivalenced REAL*8 value.  Note that a maximum of 8 characters
C        will be unpacked here, so a separate subsequent call to BUFR
C        archive library subroutine READLC will be needed to fully
C        unpack any string longer than 8 characters.

         CVAL = ' '
         KBIT = MBIT(N)
         NBT = MIN(8,NBIT(N)/8)
         CALL UPC(CVAL,NBT,MBAY(1,LUN),KBIT,.TRUE.)
         IF (NBIT(N).LE.64 .AND. ICBFMS(CVAL,NBT).NE.0) THEN
            VAL(N,LUN) = BMISS
         ELSE
            VAL(N,LUN) = RVAL
         ENDIF
      ENDIF
      ENDDO

      IBIT = NBIT(NVAL(LUN))+MBIT(NVAL(LUN))

      RETURN
      END
