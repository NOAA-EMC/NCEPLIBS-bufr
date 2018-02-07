	SUBROUTINE FDEBUFR ( ofile, tbldir, tblfil, basic )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    fdebufr
C   PRGMMR: J. Ator          ORG: NP12       DATE: 2009-07-01
C
C ABSTRACT: This subroutine reads every BUFR message from within the
C   input file that was specified on the command line.  Each such
C   message is decoded and the results are written as output to ofile.
C
C PROGRAM HISTORY LOG:
C 2009-07-01  J. Ator     Original author
C 2012-06-18  J. Ator     Added tblfil argument and options to decode
C                         files according to DX dictionary information 
C
C USAGE:    call fdebufr ( ofile, tbldir, tblfil, basic )
C   INPUT ARGUMENT LIST:
C     ofile    - character*(*): file to contain verbose output
C                listing for each decoded BUFR message
C     tbldir   - character*(*): directory containing BUFR tables
C                to be used for decoding
C     tblfil   - character*(*): file containing BUFR DX dictionary
C                information to be used for decoding.  If set to
C                'NULLFILE', then no such file will be used.
C     basic    - character: indicator as to whether the "basic"
C                option was specified on the command line:
C                  'Y' = yes
C                  'N' = no
C
C REMARKS:
C   FORTRAN logical unit numbers 51, 60, 90 and 91 are reserved for
C   use within this subroutine.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  Portable to all platforms
C
C$$$

	PARAMETER ( MXBF = 2500000 )
	PARAMETER ( MXBFD4 = MXBF/4 )
	PARAMETER ( MXDS3 = 500 )

	CHARACTER*(*)	ofile, tbldir, tblfil
	CHARACTER	basic, opened, usemt

	CHARACTER*8	cmgtag
	CHARACTER*6	cds3 ( MXDS3 )

	CHARACTER*1	bfmg ( MXBF )

	INTEGER		ibfmg ( MXBFD4 )

	EQUIVALENCE	( bfmg (1), ibfmg (1) )
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Open the output file.

	OPEN ( UNIT = 51, FILE = ofile )

C	Note that in the below OPEN statement we just need to specify
C	a dummy placeholder file.

	lunit = 60
	OPEN ( UNIT = lunit, FILE = '/dev/null' )

	CALL DATELEN ( 10 )

	nmsg = 0
	nsubt = 0

	opened = 'N'
	usemt = 'N'

	DO WHILE ( .true. )

C	    Get the next message from the input BUFR file.

	    CALL CRBMG ( bfmg, MXBF, nbyt, ierr )

	    IF ( ierr .ne. 0 )  THEN

		IF ( ierr .eq. -1 ) THEN
		    WRITE  ( UNIT = 51, FMT = '( /, 2A, I7, A, I9, A )')
     +		      'Reached end of BUFR file; it contained a total ',
     +		      'of', nmsg, ' messages and', nsubt, ' subsets'
		ELSE
		    WRITE  ( UNIT = 51, FMT = '( /, 2A, I4 )' )
     +		      'Error while reading BUFR file; the return code ',
     +		      'from CRBMG = ', IERR
		ENDIF

		IF ( basic .eq. 'N' ) THEN
		    WRITE (51, FMT = '( /, A, / )' )
     +			'Here is the DX table that was generated:'
		    CALL DXDUMP ( lunit, 51 )
		ENDIF

C		Close the output file and return.

		CLOSE ( 51 )
		RETURN
	    ENDIF

	    IF ( opened .eq. 'N' ) THEN

C		Decide how to process the file.

		IF ( IDXMSG ( ibfmg ) .eq. 1 ) THEN

C		    The first message in the file is a DX dictionary
C		    message, so assume there's an embedded table at the
C		    front of the file and use this table to decode it.

		    CALL OPENBF ( lunit, 'INUL', lunit )
		ELSE IF ( tblfil(1:8) .ne. 'NULLFILE' ) THEN

C		    A DX dictionary tables file was specified on the
C		    command line, so use it to decode the BUFR file.

		    OPEN ( UNIT = 91, FILE = tblfil )
		    CALL OPENBF ( lunit, 'IN', 91 )
		ELSE

C		    Decode the file using the master tables in tbldir.

		    usemt = 'Y'
		    CALL OPENBF ( lunit, 'SEC3', lunit )
		    CALL MTINFO ( tbldir, 90, 91 )
		ENDIF

		opened = 'Y'
	    ENDIF

	    IF ( basic .eq. 'N' ) THEN

C	        Pass the message to the decoder.

		CALL READERME ( ibfmg, lunit, cmgtag, imgdt, ierme )
	    ENDIF

C	    If this is a DX dictionary message, then don't generate any
C	    output unless master tables are being used for decoding.

	    IF (  ( IDXMSG ( ibfmg ) .ne. 1 ) .or.
     +		    ( usemt .eq. 'Y' )  ) THEN

		nmsg = nmsg + 1

		WRITE  ( UNIT = 51, FMT = '( /, A, I7 )' )
     +		    'Found BUFR message #', nmsg

		WRITE (51,*) ' ' 
		WRITE (51,*) '       Message length:',
     +					IUPBS01 ( ibfmg, 'LENM' )
		WRITE (51,*) '     Section 0 length:',
     +					IUPBS01 ( ibfmg, 'LEN0' )
		WRITE (51,*) '         BUFR edition:',
     +					IUPBS01 ( ibfmg, 'BEN' )

		WRITE (51,*) ' ' 
		WRITE (51,*) '     Section 1 length:',
     +					IUPBS01 ( ibfmg, 'LEN1' )
		WRITE (51,*) '         Master table:',
     +					IUPBS01 ( ibfmg, 'BMT' )
		WRITE (51,*) '   Originating center:',
     +					IUPBS01 ( ibfmg, 'OGCE' )
		WRITE (51,*) 'Originating subcenter:',
     +					IUPBS01 ( ibfmg, 'GSES' )
		WRITE (51,*) 'Update sequence numbr:',
     +					IUPBS01 ( ibfmg, 'USN' )
 
		IF ( IUPBS01 ( ibfmg, 'ISC2' ) .eq. 1 ) THEN
		    WRITE (51,*) '   Section 2 present?: Yes'
		ELSE
		    WRITE (51,*) '   Section 2 present?: No'
		ENDIF
 
		WRITE (51,*) '        Data category:',
     +					IUPBS01 ( ibfmg, 'MTYP' )
		WRITE (51,*) '    Local subcategory:',
     +					IUPBS01 ( ibfmg, 'MSBT' )
		WRITE (51,*) 'Internatl subcategory:',
     +					IUPBS01 ( ibfmg, 'MSBTI' )
		WRITE (51,*) ' Master table version:', 
     +					IUPBS01 ( ibfmg, 'MTV' )
		WRITE (51,*) '  Local table version:',
     +					IUPBS01 ( ibfmg, 'MTVL' )
		WRITE (51,*) '                 Year:',
     +					IUPBS01 ( ibfmg, 'YEAR' )
		WRITE (51,*) '                Month:',
     +					IUPBS01 ( ibfmg, 'MNTH' )
		WRITE (51,*) '                  Day:',
     +					IUPBS01 ( ibfmg, 'DAYS' )
		WRITE (51,*) '                 Hour:',
     +					IUPBS01 ( ibfmg, 'HOUR' )
		WRITE (51,*) '               Minute:',
     +					IUPBS01 ( ibfmg, 'MINU' )
		WRITE (51,*) '               Second:',
     +					IUPBS01 ( ibfmg, 'SECO' )
		WRITE (51,*) ' ' 

		nsub = IUPBS3 ( ibfmg, 'NSUB' )
		WRITE (51,*) 'Number of data subsets:', nsub
		nsubt = nsubt + nsub
 
		IF ( IUPBS3 ( ibfmg, 'IOBS' ) .eq. 1 ) THEN
		    WRITE (51,*) '    Data are observed?: Yes'
		ELSE
		    WRITE (51,*) '    Data are observed?: No'
		ENDIF
 
		IF ( IUPBS3 ( ibfmg, 'ICMP' ) .eq. 1 ) THEN
		    WRITE (51,*) '  Data are compressed?: Yes'
		ELSE
		    WRITE (51,*) '  Data are compressed?: No'
		ENDIF
 
		CALL UPDS3 ( ibfmg, MXDS3, cds3, nds3 )
		WRITE (51,*) ' Number of descriptors:', nds3
		DO jj = 1, nds3
		    WRITE ( 51, FMT = '( 5X, I4, A, A6)' )
     +			jj, ": ", cds3 ( jj )
		END DO

		IF (  ( basic .eq. 'N' ) .and.
     +		     ( ierme .ge. 0 )  ) THEN

C		    Decode and output the data from Section 4.

		    WRITE ( UNIT = 51,
     +			    FMT = '( /, A, I7, 3A, I10, A, I6, A )' )
     +			'BUFR message #', nmsg, ' of type ', cmgtag,
     +			' and date ', imgdt, ' contains ', nsub,
     +			' subsets:'
		    DO WHILE ( IREADSB ( lunit ) .eq. 0 )
			CALL UFDUMP ( lunit, 51 )
		    ENDDO
		ENDIF

		WRITE  ( UNIT = 51, FMT = '( /, A, I7 )' )
     +		    'End of BUFR message #', nmsg
		WRITE  ( UNIT = 51, FMT = '( /, 120("-"))' )
	    ENDIF

	ENDDO

	RETURN
	END
