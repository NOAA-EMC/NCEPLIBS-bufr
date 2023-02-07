C> @file
C> @brief Write DX BUFR tables messages to a BUFR file.
C>
C> ### Program History Log
C> Date | Programmer | Comments |
C> -----|------------|----------|
C> 2009-03-23 | J. Ator | Original author, using logic from writdx()
C> 2012-04-06 | J. Ator | Prevent storing of more than 255 Table A, Table B, or Table D descriptors in any single DX BUFR tables message
C> 2014-11-14 | J. Ator | Replace ipkm() calss with pkb() calls
C> 2014-12-10 | J. Ator | Use modules instead of COMMON blocks
C> 2022-10-04 | J. Ator | Added 8-byte wrapper
C>
C> @author J. Ator @date 2009-03-23

C> This subroutine generates one or more BUFR messages from the DX
C> BUFR tables information associated with a given BUFR file, and
C> it then writes the messages out to the same or possibly a
C> different BUFR file.
C>
C> Logical units LUNDX and LUNOT should have already been
C> opened via previous calls to subroutine openbf(), and in
C> particular logical unit LUNOT must have been opened for
C> output operations.  LUNDX and LUNOT may be the same if it is
C> desired to append to LUNOT with DX BUFR messages generated
C> from its own internal tables.
C>
C> @param[in] LUNDX - integer: Fortran logical unit number associated with
C> DX BUFR table information to be written out.
C> @param[in] LUNOT - integer: Fortran logical unit number of BUFR file to
C> which messages are to be written.
C>
C> @author J. Ator @date 2009-03-23

      RECURSIVE SUBROUTINE WRDXTB(LUNDX,LUNOT)

      USE MODV_IM8B
      USE MODA_TABABD
      USE MODA_MGWA

      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10),
     .                LD30(10),DXSTR(10)

      CHARACTER*128 BORT_STR
      CHARACTER*56  DXSTR
      CHARACTER*6   ADN30

      LOGICAL MSGFULL

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNDX,MY_LUNDX,1)
         CALL X84(LUNOT,MY_LUNOT,1)
         CALL WRDXTB(MY_LUNDX,MY_LUNOT)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  CHECK FILE STATUSES
C  -------------------

      CALL STATUS(LUNOT,LOT,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901

      CALL STATUS(LUNDX,LDX,IL,IM)
      IF(IL.EQ.0) GOTO 902

C  IF FILES ARE DIFFERENT, COPY INTERNAL TABLE
C  INFORMATION FROM LUNDX TO LUNOT
C  -------------------------------------------

      IF(LUNDX.NE.LUNOT) CALL CPBFDX(LDX,LOT)

C  GENERATE AND WRITE OUT BUFR DICTIONARY MESSAGES TO LUNOT
C  --------------------------------------------------------

      CALL DXMINI(LOT,MGWA,MBYT,MBY4,MBYA,MBYB,MBYD)

      LDA = LDXA(IDXV+1)
      LDB = LDXB(IDXV+1)
      LDD = LDXD(IDXV+1)
      L30 = LD30(IDXV+1)

C     Table A information

      DO I=1,NTBA(LOT)
      IF(MSGFULL(MBYT,LDA,MAXDX).OR.
     +    (IUPB(MGWA,MBYA,8).EQ.255)) THEN
         CALL MSGWRT(LUNOT,MGWA,MBYT)
         CALL DXMINI(LOT,MGWA,MBYT,MBY4,MBYA,MBYB,MBYD)
      ENDIF
      MBIT = 8*(MBY4-1)
      CALL PKB(IUPB(MGWA,MBY4,24)+LDA,24,MGWA,MBIT)
      MBIT = 8*(MBYA-1)
      CALL PKB(IUPB(MGWA,MBYA, 8)+  1, 8,MGWA,MBIT)
      MBIT = 8*(MBYB-1)
      CALL PKC(TABA(I,LOT),LDA,MGWA,MBIT)
      CALL PKB(          0,  8,MGWA,MBIT)
      CALL PKB(          0,  8,MGWA,MBIT)
      MBYT = MBYT+LDA
      MBYB = MBYB+LDA
      MBYD = MBYD+LDA
      ENDDO

C     Table B information

      DO I=1,NTBB(LOT)
      IF(MSGFULL(MBYT,LDB,MAXDX).OR.
     +    (IUPB(MGWA,MBYB,8).EQ.255)) THEN
         CALL MSGWRT(LUNOT,MGWA,MBYT)
         CALL DXMINI(LOT,MGWA,MBYT,MBY4,MBYA,MBYB,MBYD)
      ENDIF
      MBIT = 8*(MBY4-1)
      CALL PKB(IUPB(MGWA,MBY4,24)+LDB,24,MGWA,MBIT)
      MBIT = 8*(MBYB-1)
      CALL PKB(IUPB(MGWA,MBYB, 8)+  1, 8,MGWA,MBIT)
      MBIT = 8*(MBYD-1)
      CALL PKC(TABB(I,LOT),LDB,MGWA,MBIT)
      CALL PKB(          0,  8,MGWA,MBIT)
      MBYT = MBYT+LDB
      MBYD = MBYD+LDB
      ENDDO

C     Table D information

      DO I=1,NTBD(LOT)
      NSEQ = IUPM(TABD(I,LOT)(LDD+1:LDD+1),8)
      LEND = LDD+1 + L30*NSEQ
      IF(MSGFULL(MBYT,LEND,MAXDX).OR.
     +    (IUPB(MGWA,MBYD,8).EQ.255)) THEN
         CALL MSGWRT(LUNOT,MGWA,MBYT)
         CALL DXMINI(LOT,MGWA,MBYT,MBY4,MBYA,MBYB,MBYD)
      ENDIF
      MBIT = 8*(MBY4-1)
      CALL PKB(IUPB(MGWA,MBY4,24)+LEND,24,MGWA,MBIT)
      MBIT = 8*(MBYD-1)
      CALL PKB(IUPB(MGWA,MBYD, 8)+   1, 8,MGWA,MBIT)
      MBIT = 8*(MBYT-4)
      CALL PKC(TABD(I,LOT),LDD,MGWA,MBIT)
      CALL PKB(       NSEQ,  8,MGWA,MBIT)
         DO J=1,NSEQ
         JJ  = LDD+2 + (J-1)*2
         IDN = IUPM(TABD(I,LOT)(JJ:JJ),16)
         CALL PKC(ADN30(IDN,L30),L30,MGWA,MBIT)
         ENDDO
      MBYT = MBYT+LEND
      ENDDO

C     Write the unwritten (leftover) message.

      CALL MSGWRT(LUNOT,MGWA,MBYT)

C     Write out one additional (dummy) DX message containing zero
C     subsets.  This will serve as a delimiter for this set of
C     table messages within output unit LUNOT, just in case the
C     next thing written to LUNOT ends up being another set of
C     table messages.

      CALL DXMINI(LOT,MGWA,MBYT,MBY4,MBYA,MBYB,MBYD)
      CALL GETLENS(MGWA,2,LEN0,LEN1,LEN2,L3,L4,L5)
      MBIT = (LEN0+LEN1+LEN2+4)*8
      CALL PKB(0,16,MGWA,MBIT)
      CALL MSGWRT(LUNOT,MGWA,MBYT)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: WRDXTB - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: WRDXTB - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
902   CALL BORT('BUFRLIB: WRDXTB - DX TABLE FILE IS CLOSED, IT '//
     . 'MUST BE OPEN')
      END
