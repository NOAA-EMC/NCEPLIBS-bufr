C       This is a test for NCEPLIBS-bufr.
C
C       This tests a number of library routines and functionality
C       involved in writing output BUFR messages, as noted in the
C       comments and print statements below.
C
C       Jeff Ator

#ifdef INTSIZE_8
        INTEGER*4       IREADNS
#endif

        INTEGER         jdate(5), jdump(5)

        CHARACTER       cmgtag*8, tabdb(1000)*128

C*----------------------------------------------------------------------

        print *, '----------------------------------------------------'
        print *, 'testing BUFRLIB: writing OUT_5'
        print *, '  using DUMPBF and GETABDB'
        print *, '  using UFDUMP, UFBDMP and DXDUMP'
        print *, '----------------------------------------------------'

#ifdef INTSIZE_8
        CALL SETIM8B ( .true. )
#endif

C*      Open the output log (ASCII) file.

        OPEN ( UNIT = 13, FILE = 'out5.bufr' )

C*      Make a "FIRST" call to subroutine OPENBF to dynamically
C*      allocate internal arrays, in case this code is being run to
C*      test a DA build.  Otherwise, the below call to subroutine
C*      DUMPBF will fail when trying to call subroutine STATUS,
C*      because subroutine OPENBF won't yet have been called.

        CALL OPENBF ( 13, 'FIRST', 13 )

C*      Open the input (BUFR) file.  Note that since we're about to
C*      call subroutine DUMPBF for this file, then we don't need to
C*      first call subroutine OPENBF for this file, because subroutine
C*      DUMPBF will do that internally.

        OPEN ( UNIT = 11, FILE = 'testfiles/OUT_5_infile' )

        CALL DATELEN ( 10 )
        print *, '        DATELEN'

        WRITE ( 13, FMT = '(///,A)' ) '------------ DUMPBF ------------'
        CALL DUMPBF ( 11, jdate, jdump )
        print *, '        DUMPBF'
        WRITE ( 13, FMT = '(A,5I5)' ) 'jdate =', (jdate(ii), ii=1,5)
        WRITE ( 13, FMT = '(A,5I5)' ) 'jdump =', (jdump(ii), ii=1,5)

C*      Subroutine DUMPBF will have just closed the input (BUFR) file
C*      with an internal call to subroutine CLOSBF (which also does an
C*      internal Fortran CLOSE on the logical unit number), so we now
C*      need to reopen the file with a new call to subroutine OPENBF.

        OPEN ( UNIT = 11, FILE = 'testfiles/OUT_5_infile' )
        CALL OPENBF ( 11, 'IN', 11 )
        print *, '        OPENBF'

        WRITE ( 13, FMT = '(///,A)' ) '------------ GETABDB -----------'
        CALL GETABDB ( 11, tabdb, 1000, jtab )
        print *, '        GETABDB'
        DO ii = 1, jtab
          WRITE ( 13, FMT = '(A,I4,2A)' )
     +      'tabdb entry #', ii, ":", tabdb(ii)
        END DO

        WRITE ( 13, FMT = '(///,A,/)' ) '----------- DXDUMP -----------'
        CALL DXDUMP ( 11, 13 )
        print *, '        DXDUMP'

        print *, '        IREADNS'
        print *, '        UFDUMP'
        print *, '        UFBDMP'
        nsub = 0
        DO WHILE ( IREADNS ( 11, cmgtag, imgdt ) .eq. 0 )
          nsub = nsub + 1
          WRITE ( 13, FMT = '(///,A,I1,A)' )
     +      '------------------------------ SUBSET #', nsub,
     +      '------------------------------'
          WRITE ( 13, FMT = '(//,A)' )
     +      '------------ UFDUMP ------------'
          CALL UFDUMP ( 11, 13 )
          WRITE ( 13, FMT = '(//,A)' )
     +      '------------ UFBDMP ------------'
          CALL UFBDMP ( 11, 13 )
        END DO

        STOP
        END
