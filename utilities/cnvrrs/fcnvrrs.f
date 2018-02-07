	SUBROUTINE FCNVRRS  ( mtbldir, tablefil, outfile )

	CHARACTER*(*)	mtbldir, tablefil, outfile

	PARAMETER	( MXBF = 50000 )
	PARAMETER	( MXDS3 = 10 )

	LOGICAL		exists

        CHARACTER       cbfmg*(MXBF), cmgtag*8, cds3(MXDS3)*6

        INTEGER         ibfmg ( MXBF / 4 )

        EQUIVALENCE     ( cbfmg (1:4), ibfmg (1) )
C*-----------------------------------------------------------------------
	iret = 0

	iunmt1 = 11
	iunmt2 = 12
	iunrrs = 13
	iuntbl = 14
	iunout = 51

C*	Set up the input RRS BUFR file stream.

	OPEN ( UNIT = iunrrs, FILE = '/dev/null' )
	CALL OPENBF( iunrrs, 'SEC3', iunrrs )
	IF ( mtbldir(1:8) .ne. '%NULLDIR' )
     +	    CALL MTINFO ( mtbldir, iunmt1, iunmt2 )

C*	Open the input DX table file.

	INQUIRE ( FILE = tablefil, EXIST = exists )
	IF ( .not. exists ) THEN
	  PRINT *, 'ERROR: COULD NOT FIND DX TABLE FILE ', tablefil
	  RETURN
	ENDIF
	OPEN ( iuntbl, FILE = tablefil, IOSTAT = ier )
	IF ( ier .ne. 0 ) THEN
	  PRINT *, 'ERROR: COULD NOT OPEN DX TABLE FILE ', tablefil
          RETURN
        ENDIF

C*	Open the output BUFR file.  Note that we already know the
C*	directory is writable from the calling program.

	OPEN ( iunout, FILE = outfile, FORM = 'UNFORMATTED' )
        CALL OPENBF ( iunout, 'NODX', iuntbl )

C*	Specify that output BUFR messages should be WMO-standard,
C*	coded using BUFR edition 4, and as long as necessary up to
C*	the internal BUFRLIB limit of MXMSGL.

	CALL STDMSG ( 'Y' )
	CALL PKVS01 ( 'BEN', 4 )
        CALL MAXOUT ( 0 )

C*	Since this program may generate BUFR output subsets larger than
C*	65530 bytes, and since BUFRLIB will write such subsets into
C*	their own BUFR messages within the same internal call to
C*	subroutine MSGUPD, then it is possible for an empty BUFR message
C*	(i.e. containing 0 subsets) to be subsequently generated, so we
C*	need to tell BUFRLIB to never write any such empty messages to
C*	the output file.

        CALL CLOSMG ( (-1) * iunout )

	DO WHILE ( .true. )

C*	  Get the next message from the RRS BUFR file.

	  CALL CRBMG ( cbfmg, MXBF, nbyt, ierr )
	  IF ( ierr .ne. 0 ) THEN

C*	    There are no more messages to be read.

	    IF ( ierr .ne. -1 ) PRINT *, 'ERROR CODE ', ierr,
     +		' WHILE READING FROM RRS BUFR FILE'

C*	    Now, combine the saved levels from all of the different
C*	    messages that were read into one complete merged and
C*	    sorted sounding.

	    CALL MRGLVLS

C*	    Write the output.

	    CALL WRITE52 ( iunout )

	    CALL CLOSBF ( iunout )
	    RETURN
	  ENDIF

	  IF ( IDXMSG ( ibfmg ) .ne. 1 ) THEN

C*	    What type of data does this message contain?

	    CALL UPDS3 ( ibfmg, MXDS3, cds3, nds3 )
	    IF ( nds3 .gt. 0 ) THEN
	      READ ( UNIT = cds3(1)(5:6), FMT = '(I2)' ) iyyy
	      IF ( ( iyyy .lt. 61 ) .or. ( iyyy .gt. 63 ) ) THEN

		CALL READERME ( ibfmg, iunrrs, cmgtag, imgdt, ierme )
		IF ( ierme .eq. 0 ) THEN

C*		  Read and store the subsets from this message.  Each
C*		  RRS subset contains data at a single vertical level.

		  IF ( iyyy .eq. 60 ) THEN
		    CALL READR60 ( iunrrs )
		  ELSE IF ( iyyy .eq. 64 ) THEN
		    CALL READR64 ( iunrrs )
		  ELSE IF ( iyyy .eq. 65 ) THEN
		    CALL READR65 ( iunrrs )
		  ELSE IF ( iyyy .eq. 66 ) THEN
		    CALL READR66 ( iunrrs )
		  ENDIF
		ENDIF
	      ENDIF
	    ENDIF
	  ENDIF

	ENDDO

        RETURN
        END
