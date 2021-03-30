C> @file
C> @brief Write a long character string (greater than 8 bytes) to
C> a data subset.

C> This subroutine writes a long character string (greater than 8 bytes)
C> to a data subset.
C>
C> <p>The data subset should have already been written into a BUFR
C> message via a previous call to subroutine writsb(), writsa() or
C> equivalent, before calling this subroutine to write any long
C> character strings into the same subset.
C>
C> @authors J. Woollen
C> @authors J. Ator
C> @date 2003-11-04
C>
C> @param[in] LUNIT  - integer: Fortran logical unit number for BUFR file
C> @param[in] CHR   - character*(*): Value corresponding to STR
C> @param[in] STR    - character*(*): Table B mnemonic of long character
C>                     string to be written, possibly supplemented
C>                     with an ordinal occurrence notation
C>
C> <p>If there is more than one occurrence of STR within the data subset
C> definition, then each occurrence can be written via a separate call
C> to this subroutine, and by appending the ordinal number of the
C> occurrence to STR in each case.  For example, if there are 5
C> occurrences of mnemonic LSTID within a given data subset definition,
C> then 5 separate calls should be made to this subroutine, once each
C> with STR set to 'LSTID#1', 'LSTID#2', 'LSTID#3', 'LSTID#4' and
C> 'LSTID#5'.  However, the first notation is superfluous, because
C> omitting the ordinal number always defaults to the first occurrence
C> of a particular string, so a user could just specify 'LSTID'
C> instead of 'LSTID#1'.
C>
C> @remarks
C> - Character strings which are 8 bytes or less in length can be
C> written by converting the string into a real*8 value within the
C> application program, and then using the real*8 USR array within a
C> call to subroutine ufbint(), ufbrep(), ufbseq() or equivalent
C> prior to calling subroutine writsb() or writsa() for the data
C> subset.
C>
C> <b>Program history log:</b>
C> - 2003-11-04  J. Woollen -- Original author
C> - 2004-08-09  J. Ator    -- Maximum message length increased from
C>                             20,000 to 50,000 bytes
C> - 2005-11-29  J. Ator    -- Use getlens()
C> - 2007-01-19  J. Ator    -- Replaced call to parseq with call to
C>                             parstr()
C> - 2009-03-23  J. Ator    -- Added '#' option for more than one
C>                             occurrence of STR
C> - 2009-08-11  J. Woollen -- Added COMMON COMPRS along with logic to
C>                             write long strings into compressed subsets
C> - 2012-12-07  J. Ator    -- Allow str mnemonic length of up to 14 chars
C>                             when used with '#' occurrence code
C> - 2014-10-22  J. Ator    -- No longer abort if no subset available for
C>                             writing; just print a warning message
C> - 2014-12-10  J. Ator    -- USE modules instead of COMMON blocks
C> - 2020-09-09  J. Ator    -- No longer abort if STR not available within
C>                             subset definition; instead, just print a
C>                             warning message
C>
      SUBROUTINE WRITLC(LUNIT,CHR,STR)

      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES
      USE MODA_COMPRS

      COMMON /QUIET / IPRT

      CHARACTER*(*) CHR,STR
      CHARACTER*128 BORT_STR
      CHARACTER*128 ERRSTR
      CHARACTER*10  CTAG
      CHARACTER*14  TGS(10)

      DATA MAXTG /10/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check the file status.

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C     Check for tags (mnemonics) in input string (there can only be one)

      CALL PARSTR(STR,TGS,MAXTG,NTG,' ',.TRUE.)
      IF(NTG.GT.1) GOTO 903

C     Check if a specific occurrence of the input string was requested;
C     if not, then the default is to write the first occurrence.

      CALL PARUTG(LUN,1,TGS(1),NNOD,KON,ROID)
      IF(KON.EQ.6) THEN
         IOID=NINT(ROID)
         IF(IOID.LE.0) IOID = 1
         CTAG = ' '
         II = 1
         DO WHILE((II.LE.10).AND.(TGS(1)(II:II).NE.'#'))
            CTAG(II:II)=TGS(1)(II:II)
            II = II + 1
         ENDDO
      ELSE
         IOID = 1
         CTAG = TGS(1)(1:10)
      ENDIF

      IF(IUPBS3(MBAY(1,LUN),'ICMP').GT.0) THEN       

C        The message is compressed.

         N = 1
         ITAGCT = 0
         CALL USRTPL(LUN,N,N)
         DO WHILE (N+1.LE.NVAL(LUN))
            N = N+1
            NODE = INV(N,LUN)
            IF(ITP(NODE).EQ.1) THEN
               CALL USRTPL(LUN,N,MATX(N,NCOL))
            ELSEIF(CTAG.EQ.TAG(NODE)) THEN
               ITAGCT = ITAGCT + 1
               IF(ITAGCT.EQ.IOID) THEN 
                  IF(ITP(NODE).NE.3) GOTO 904
                  CATX(N,NCOL)=' '

C                 The following statement enforces a limit of MXLCC 
C                 characters per long character string when writing
C                 compressed messages.  This limit keeps the array
C                 CATX to a reasonable dimensioned size. 

                  NCHR=MIN(MXLCC,IBT(NODE)/8)
                  CATX(N,NCOL)=CHR(1:NCHR)
                  CALL USRTPL(LUN,1,1)
                  GOTO 100
               ENDIF
            ENDIF
         ENDDO
      ELSE

C        The message is not compressed. Locate the beginning of the
C        data (Section 4) in the message.

         CALL GETLENS(MBAY(1,LUN),3,LEN0,LEN1,LEN2,LEN3,L4,L5)
         MBYTE = LEN0 + LEN1 + LEN2 + LEN3 + 4
         NSUBS = 1

C        Find the most recently written subset in the message.

         DO WHILE(NSUBS.LT.NSUB(LUN))
            IBIT = MBYTE*8
            CALL UPB(NBYT,16,MBAY(1,LUN),IBIT)
            MBYTE = MBYTE + NBYT
            NSUBS = NSUBS + 1
         ENDDO

         IF(NSUBS.NE.NSUB(LUN)) THEN
            IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: WRITLC - COULDN''T WRITE VALUE FOR ' // CTAG
     . // ' INTO SUBSET, BECAUSE NO SUBSET WAS OPEN FOR WRITING'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
            ENDIF
            GOTO 100
         ENDIF

C        Locate and write the long character string within this subset.

         ITAGCT = 0
         MBIT = MBYTE*8 + 16
         NBIT = 0
         N = 1
         CALL USRTPL(LUN,N,N)
         DO WHILE (N+1.LE.NVAL(LUN))
            N = N+1
            NODE = INV(N,LUN)
            MBIT = MBIT+NBIT
            NBIT = IBT(NODE)
            IF(ITP(NODE).EQ.1) THEN
               CALL UPBB(IVAL,NBIT,MBIT,MBAY(1,LUN))
               CALL USRTPL(LUN,N,IVAL)
            ELSEIF(CTAG.EQ.TAG(NODE)) THEN
               ITAGCT = ITAGCT + 1
               IF(ITAGCT.EQ.IOID) THEN 
                 IF(ITP(NODE).NE.3) GOTO 904
                 NCHR = NBIT/8
                 IBIT = MBIT
                 DO J=1,NCHR
                   CALL PKC(' ',1,MBAY(1,LUN),IBIT)
                 ENDDO
                 CALL PKC(CHR,NCHR,MBAY(1,LUN),MBIT)
                 CALL USRTPL(LUN,1,1)
                 GOTO 100
               ENDIF
            ENDIF
         ENDDO
      ENDIF

      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: WRITLC - COULDN''T WRITE VALUE FOR ' // CTAG
     . // ' INTO SUBSET, BECAUSE IT WASN''T FOUND IN THE SUBSET'
     . // ' DEFINITION'
      CALL ERRWRT(ERRSTR)
      ERRSTR = '(' // CTAG // ' MAY NOT BE IN THE BUFR TABLE(?))'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
902   CALL BORT('BUFRLIB: WRITLC - A MESSAGE MUST BE OPEN IN OUTPUT '//
     . 'BUFR FILE, NONE ARE')
903   WRITE(BORT_STR,'("BUFRLIB: WRITLC - THERE CANNOT BE MORE THAN '//
     . ' ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE",I4'//
     . ',")")') STR,NTG
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: WRITLC - MNEMONIC ",A," DOES NOT '//
     . 'REPRESENT A CHARACTER ELEMENT (TYP=",A,")")') CTAG,TYP(NODE)
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: WRITLC - THE MOST RECENTLY WRITTEN '//
     . ' SUBSET NO. (",I3,") IN MSG .NE. THE STORED VALUE FOR THE NO.'//
     . ' OF SUBSETS (",I3,") IN MSG")') NSUBS,NSUB(LUN)
      CALL BORT(BORT_STR)
      END
