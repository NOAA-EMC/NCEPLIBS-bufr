        PARAMETER ( MXVAL1 = 200 )
        PARAMETER ( MXVAL2 = 12 )
        PARAMETER ( MXLVL = 4490 )

	REAL*8		r8arr1 ( MXVAL1 ), r8arr2 ( MXVAL2, MXLVL )

        PARAMETER ( MXBFMG = 50000 )

        INTEGER         mgbf ( MXBFMG )

	CHARACTER	cmgtag*8, smid*9, dummystr*9

C*----------------------------------------------------------------------

	print *, '----------------------------------------------------'
	print *, 'testing BUFRLIB: writing OUT_4'
        print *, '  using dynamic allocation'
	print *, '  using OPENBF IO = ''NODX'' and IO = ''QUIET'''
	print *, '  using STRCPT, WRDXTB and WRITSA'
	print *, '----------------------------------------------------'

        CALL ISETPRM ( 'NFILES', 4 )
        CALL ISETPRM ( 'MXMSGL', 400000 )
        CALL ISETPRM ( 'MAXSS', 250000 )
        CALL ISETPRM ( 'MAXMEM', 100000 )
        CALL ISETPRM ( 'MAXMSG', 100 )
        CALL ISETPRM ( 'MXDXTS', 5 )
        CALL ISETPRM ( 'MXCDV', 100 )
        CALL ISETPRM ( 'MXCSB', 100 )
        CALL ISETPRM ( 'MXLCC', 8 )
	print *, '        ISETPRM'

C*	Open the BUFR input and output files.

	OPEN ( UNIT = 11, FILE = 'testfiles/OUT_4_infile1' )
	OPEN ( UNIT = 12, FILE = 'testfiles/OUT_4_infile2' )
	OPEN ( UNIT = 13, FILE = 'out4.bufr', FORM ='UNFORMATTED' )

	CALL OPENBF ( 11, 'IN', 11 )
	CALL OPENBF ( 12, 'SEC3', 12 )
	CALL OPENBF ( 13, 'NODX', 11 )
	CALL OPENBF ( 13, 'QUIET', -1 )
	print *, '        OPENBF'

        CALL MTINFO ( '../install/tables', 90, 91 )
	print *, '        MTINFO'

        CALL MAXOUT ( MXBFMG*4 )
	print *, '        MAXOUT'

C*      The following will ensure that subroutine STNDRD is called
C*      internally during the subsequent calls to WRITSB and CLOSMG.

        CALL STDMSG ('Y')
        print *, '        STDMSG'

C*      Append a (tank) receipt time to Section 1 of each output message

	CALL STRCPT ( 'Y', 2020, 11, 4, 15, 29 )
	print *, '        STRCPT'

C*	Process 1 message with 1 subset from infile1.

        CALL READMG ( 11, cmgtag, imgdt, iermg )
	print *, '        READMG'

        IF ( iermg .eq. 0 ) THEN
          CALL READSB ( 11, iersb )
	  print *, '        READSB'

          IF ( iersb .eq. 0 ) THEN
	    CALL OPENMB ( 13, 'NC007000', 2020022514 )
	    print *, '        OPENMB'

            CALL UFBSEQ ( 11, r8arr1, MXVAL1, 1, nlv, 'NC007000' )
            CALL UFBSEQ ( 13, r8arr1, MXVAL1, 1, nlv, 'NC007000' )
	    print *, '        UFBSEQ'

            CALL WRITSB ( 13 )
          END IF
        END IF

	CALL CLOSMG ( 13 )
	print *, '        CLOSMG'

C*	Process 1 message with 4 subset from infile2.

        CALL READMG ( 12, cmgtag, imgdt, iermg )

C*      Turn off output message standardization.

        CALL STDMSG ('N')
        
C*      Write DX table information for this message into the
C*      output file.

        CALL WRDXTB ( 12, 13 )
	print *, '        WRDXTB'

        IF ( iermg .eq. 0 ) THEN

          nsub = 0

          DO WHILE ( IREADSB ( 12 ) .eq. 0 )

            nsub = nsub + 1

            CALL UFBSEQ ( 12, r8arr1, MXVAL1, 1, nlv, 'DATETMLN' )
            ibfdt = ( IDNINT(r8arr1(2)) * 1000000 ) +
     +              ( IDNINT(r8arr1(3)) * 10000 ) +
     +              ( IDNINT(r8arr1(4)) * 100 ) + IDNINT(r8arr1(5))
	    CALL OPENMB ( 13, 'MSTTB001', ibfdt )
            CALL UFBSEQ ( 13, r8arr1, MXVAL1, 1, nlv, 'DATETMLN' )

            WRITE ( UNIT = smid, FMT = '(A,I1.1)' ) 'STATION#', nsub
            IF ( nsub .eq. 1 ) THEN
              CALL READLC ( 12, dummystr, 'DUMMYSTR' )
              IF ( ICBFMS( dummystr, 9 ) .eq. 0 ) smid = dummystr
            END IF

            CALL UFBSEQ ( 12, r8arr1, MXVAL1, 1, nlv, 'IDLSIPTM' )
            CALL UFBSEQ ( 13, r8arr1, MXVAL1, 1, nlv, 'IDLSIPTM' )
            CALL UFBSEQ ( 12, r8arr1, MXVAL1, 1, nlv, 'HAVCOLS' )
            CALL UFBSEQ ( 13, r8arr1, MXVAL1, 1, nlv, 'HAVCOLS' )
            CALL UFBSEQ ( 12, r8arr1, MXVAL1, 1, nlv, 'CLINRVSD' )
            CALL UFBSEQ ( 13, r8arr1, MXVAL1, 1, nlv, 'CLINRVSD' )
            CALL UFBSEQ ( 12, r8arr2, MXVAL2, MXLVL, nlv2, 'TDWPRAOB' )

            CALL DRFINI ( 13, nlv2, 1, '(TDWPRAOB)' )
            CALL UFBSEQ ( 13, r8arr2, MXVAL2, nlv2, nlv, 'TDWPRAOB' )

            CALL HOLD4WLC ( 13, smid, 'SMID' )
            CALL WRITSA ( 13, MXBFMG, mgbf, lmgbf )
            IF ( nsub .eq. 1 ) THEN
              CALL WRITLC ( 13, dummystr, 'DUMMYSTR' )
            END IF
           
          END DO

          CALL WRITSA ( -13, MXBFMG, mgbf, lmgbf )

          print *, '        ICBFMS'
          print *, '        HOLD4WLC'
          print *, '        DRFINI'
          print *, '        WRITSA'

        END IF
 
        CALL CLOSBF ( 13 )
        print *, '        CLOSBF'

	STOP
	END
