C> @file
C> @brief Print the contents of a data subset.

C> This subroutine prints a verbose listing of the contents of a data
C> subset, including all data values and replicated sequences, as well
C> as jump/link table information and other internal subset pointers.
C>
C> <p>This subroutine is similar to subroutine ufdump(), but it prints
C> different characteristics of each data subset, and in a slightly
C> different format.  However, both subroutines can be useful for
C> different diagnostic purposes, and both can also be run
C> interactively to scroll through the contents of a data subset.
C>
C> @authors J. Woollen
C> @authors J. Ator
C> @authors D. Keyser
C> @date 1994-01-06
C>
C> @param[in] LUNIN   -- integer: Absolute value is Fortran logical
C>                       unit number for BUFR file 
C>                       - If LUNIN > 0, data values are printed to
C>                         LUPRT using the format descriptor code
C>                         'G15.6', meaning that all values will be
C>                         printed (since the format adapts to the
C>                         order of magnitude of each value), but
C>                         values won't necessarily be lined up
C>                         with the decimal point in the same column
C>                       - If LUNIN < 0, data values are printed to
C>                         LUPRT using the format descriptor code
C>                         'F15.6', meaning that all values will be
C>                         lined up with the decimal point in the
C>                         same column, but values exceeding the
C>                         format width of 15 characters will print
C>                         as overflow (e.g. '***************')
C> @param[in] LUPRT   -- integer: Fortran logical unit number for
C>                       print output
C>                       - 0 = Run interactively, printing to
C>                             standard output
C>
C> <p>Logical unit ABS(LUNIN) should have already been opened for
C> input operations via a previous call to subroutine openbf(), and a
C> BUFR data subset should have already been read into internal arrays
C> via a previous call to one of the
C> [subset-reading subroutines](@ref hierarchy).
C>
C> <p>Except when LUPRT = 0, logical unit LUPRT must already be
C> associated with a filename on the local system, typically via a
C> Fortran "OPEN" statement.  When LUPRT = 0, the subroutine will run
C> interactively and print to standard output, scrolling 20 lines at
C> a time and prompting each time whether to quit and return to the
C> application program (by typing 'q' then '<Enter>') or continue
C> scrolling (by typing anything else).
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
C> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
C> | 2002-05-14 | J. Woollen | Removed old Cray compiler directives |
C> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
C> | 2003-11-04 | D. Keyser  | Increased MAXJL from 15000 to 16000; unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
C> | 2003-11-04 | D. Keyser  | Allowed fuzziness in test for missing values; added option to print using either 'G15.6' or 'F15.6'; added several jump/link table values to output |
C> | 2004-08-18 | J. Ator    | Modified fuzziness test; added readlc() option; restructured some logic for clarity |
C> | 2006-04-14 | D. Keyser  | Add call to upftbv() for flag tables to get actual bits that were set to generate value |
C> | 2007-01-19 | J. Ator    | Use function ibfms() |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2020-09-09 | J. Ator    | Fix missing check for long character strings |
C> | 2021-09-30 | J. Ator    | Replace rjust with Fortran intrinsic adjustr |
C>
      SUBROUTINE UFBDMP(LUNIN,LUPRT)

      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_TABABD
      USE MODA_TABLES

      CHARACTER*120 LCHR2
      CHARACTER*20 LCHR,PMISS
      CHARACTER*14 BITS
      CHARACTER*10 TG,TG_RJ
      CHARACTER*8  VC
      CHARACTER*7  FMTF
      CHARACTER*3  TP
      CHARACTER*1  TAB,YOU
      EQUIVALENCE  (VL,VC)
      REAL*8       VL

      PARAMETER (MXFV=31)
      INTEGER	IFV(MXFV)

      DATA PMISS /'             MISSING'/
      DATA YOU /'Y'/

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IF(LUPRT.EQ.0) THEN
         LUOUT = 6
      ELSE
         LUOUT = LUPRT
      ENDIF

C  CHECK THE FILE STATUS AND I-NODE
C  --------------------------------

      LUNIT = ABS(LUNIN)
      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902
      IF(INODE(LUN).NE.INV(1,LUN)) GOTO 903

C  DUMP THE CONTENTS OF MODULE USRINT FOR UNIT ABS(LUNIN)
C  ------------------------------------------------------

      DO NV=1,NVAL(LUN)
      IF(LUPRT.EQ.0 .AND. MOD(NV,20).EQ.0) THEN

C  When LUPRT=0, the output will be scrolled, 20 elements at a time
C  ----------------------------------------------------------------

         PRINT*,'(<enter> for MORE, q <enter> to QUIT)'
         READ(5,'(A1)') YOU

C  If the terminal enters "q" followed by "<enter>" after the prompt
C  "(<enter> for MORE, q <enter> to QUIT)", scrolling will end and the
C  subroutine will return to the calling program
C  -------------------------------------------------------------------

         IF(YOU.EQ.'q') THEN
         PRINT*
         PRINT*,'==> You have chosen to stop the dumping of this subset'
         PRINT*
            GOTO 100
         ENDIF
      ENDIF
      ND = INV (NV,LUN)
      VL = VAL (NV,LUN)
      TG = TAG (ND)
      TP = TYP (ND)
      IT = ITP (ND)
      IB = IBT (ND)
      IS = ISC (ND)
      IR = IRF (ND)
      JP = JUMP(ND)
      LK = LINK(ND)
      JB = JMPB(ND)
      TG_RJ = ADJUSTR(TG)
      IF(TP.NE.'CHR') THEN
         BITS = '              '
         IF(IT.EQ.2) THEN
            CALL NEMTAB(LUN,TG,IDN,TAB,N)
            IF(TABB(N,LUN)(71:75).EQ.'FLAG') THEN

C              Print a listing of the bits corresponding to
C              this value.

               CALL UPFTBV(LUNIT,TG,VL,MXFV,IFV,NIFV)
               IF(NIFV.GT.0) THEN
                  BITS(1:1) = '('
                  IPT = 2
                  DO II=1,NIFV
                    ISZ = ISIZE(IFV(II))
                    WRITE(FMTF,'(A2,I1,A4)') '(I', ISZ, ',A1)'
                    IF((IPT+ISZ).LE.14) THEN
                       WRITE(BITS(IPT:IPT+ISZ),FMTF) IFV(II), ','
                       IPT = IPT + ISZ + 1
                    ELSE
                       BITS(2:13) = 'MANY BITS ON'
                       IPT = 15
                    ENDIF
                  ENDDO
                  BITS(IPT-1:IPT-1) = ')'
               ENDIF
            ENDIF
         ENDIF
         IF(IBFMS(VL).NE.0) THEN
            WRITE(LUOUT,2) NV,TP,IT,TG_RJ,PMISS,IB,IS,IR,ND,JP,LK,JB
         ELSE
            IF(LUNIT.EQ.LUNIN) THEN
               WRITE(LUOUT,1) NV,TP,IT,TG_RJ,VL,BITS,IB,IS,IR,ND,JP,LK,
     .          JB
            ELSE
               WRITE(LUOUT,10) NV,TP,IT,TG_RJ,VL,BITS,IB,IS,IR,ND,JP,LK,
     .          JB
            ENDIF
         ENDIF
      ELSE
         NCHR=IB/8
         IF(NCHR.GT.8) THEN
            CALL READLC(LUNIT,LCHR2,TG_RJ)
            IF (ICBFMS(LCHR2,NCHR).NE.0) THEN
               LCHR = PMISS
            ELSE
               LCHR = LCHR2(1:20)
            ENDIF
         ELSE
            IF(IBFMS(VL).NE.0) THEN
               LCHR = PMISS
            ELSE
               LCHR = VC
            ENDIF
         ENDIF
         IF ( NCHR.LE.20 .OR. LCHR.EQ.PMISS ) THEN
            LCHR = ADJUSTR(LCHR)
            WRITE(LUOUT,2) NV,TP,IT,TG_RJ,LCHR,IB,IS,IR,ND,JP,LK,JB
         ELSE
            WRITE(LUOUT,4) NV,TP,IT,TG_RJ,LCHR2(1:NCHR),IB,IS,IR,ND,JP,
     .       LK,JB
         ENDIF
      ENDIF
      ENDDO

      WRITE(LUOUT,3)

1     FORMAT(I5,1X,A3,'-',I1,1X,A10,5X,G15.6,1X,A14,7(1X,I5))
10    FORMAT(I5,1X,A3,'-',I1,1X,A10,5X,F15.6,1X,A14,7(1X,I5))
2     FORMAT(I5,1X,A3,'-',I1,1X,A10,1X,   A20,  14X,7(1X,I5))
3     FORMAT(/' >>> END OF SUBSET <<< '/)
4     FORMAT(I5,1X,A3,'-',I1,1X,A10,1X,      A,     7(1X,I5))

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: UFBDMP - INPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: UFBDMP - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: UFBDMP - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
903   CALL BORT('BUFRLIB: UFBDMP - LOCATION OF INTERNAL TABLE FOR '//
     . 'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN '//
     . 'INTERNAL SUBSET ARRAY')
      END
