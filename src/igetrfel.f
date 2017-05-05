	INTEGER FUNCTION IGETRFEL ( N, LUN )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    IGETRFEL
C   PRGMMR: J. ATOR          ORG: NCEP       DATE: 2016-05-27
C
C ABSTRACT: THIS FUNCTION CHECKS WHETHER THE INPUT ELEMENT REFERS TO
C   A PREVIOUS ELEMENT WITHIN THE SAME SUBSET VIA AN INTERNAL BITMAP.
C   IF SO, THEN THE REFERENCED ELEMENT IS RETURNED.  IN ADDITION, IF
C   THE INPUT ELEMENT IS A 2-XX-255 MARKER OPERATOR, ITS SCALE FACTOR,
C   BIT WIDTH AND REFERENCE VALUE ARE SET INTERNALLY TO MATCH THOSE
C   OF THE REFERENCED ELEMENT.
C
C PROGRAM HISTORY LOG:
C 2016-05-27  J. ATOR    -- ORIGINAL AUTHOR
C 2017-04-03  J. ATOR    -- ADD A DIMENSION TO ALL TCO ARRAYS SO THAT
C                           EACH SUBSET DEFINITION IN THE JUMP/LINK
C                           TABLE HAS ITS OWN SET OF TABLE C OPERATORS
C
C USAGE:    CALL IGETRFEL ( N, LUN )
C   INPUT ARGUMENT LIST:
C     N        - INTEGER: SUBSET ELEMENT
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C   OUTPUT ARGUMENT LIST:
C     IGETRFEL - INTEGER: SUBSET ELEMENT REFERENCED BY ELEMENT N
C                WITHIN THE SAME SUBSET
C                  0 = INPUT ELEMENT DOES NOT REFER TO A PREVIOUS
C                      ELEMENT, OR REFERENCED ELEMENT NOT FOUND
C
C REMARKS:
C    THIS ROUTINE CALLS:        ADN30    BORT     IBFMS    IMRKOPR
C                               LSTJPB   NEMTAB
C    THIS ROUTINE IS CALLED BY: RCSTPL   RDCMPS
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	USE MODA_MSGCWD
	USE MODA_USRINT
	USE MODA_TABLES
	USE MODA_BITMAPS
	USE MODA_NRV203

	INCLUDE 'bufrlib.prm'

	CHARACTER*128	BORT_STR
	CHARACTER*6	CFLWOPR,ADN30,FXY
	CHARACTER*1	TAB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	IGETRFEL = 0

	NODE = INV( N, LUN )

	IF ( ITP(NODE) .GT. 1 ) THEN
	    IF ( NODE .EQ. LSTNOD ) THEN
		LSTNODCT = LSTNODCT + 1
	    ELSE
		LSTNOD = NODE
		LSTNODCT = 1
	    END IF
C
C	    Does this subset definition contain any Table C operators
C	    with an X value of 21 or greater?
C
	    IDXTA = 0
	    IF ( NTAMC .GT. 0 ) THEN
		NODTAM = LSTJPB( NODE, LUN, 'SUB' )
		DO II = 1, NTAMC
		  IF ( NODTAM .EQ. INODTAMC(II) ) THEN
		    IDXTA = II
		    NTC = NTCO(II)
		  END IF
		END DO
	    END IF
	    IF ( ( IDXTA .GT. 0 ) .AND. ( NBTM .GT. 0 ) ) THEN

C		Check whether this element references a previous element
C		in the same subset via an internal bitmap.  To do this,
C		we first need to determine the appropriate "follow"
C		operator (if any) corresponding to this element.

		CFLWOPR = 'XXXXXX'
		IF ( IMRKOPR(TAG(NODE)) .EQ. 1 ) THEN
		  CFLWOPR = TAG(NODE)(1:3) // '000'
		ELSE
		  CALL NEMTAB( LUN, TAG(NODE), IDN, TAB, NN )
		  IF ( TAB .EQ. 'B' ) THEN
		    FXY = ADN30(IDN,6)
		    IF ( FXY(2:3) .EQ. '33' ) CFLWOPR = '222000'
		  END IF
		END IF
		IF ( CFLWOPR .EQ. 'XXXXXX' ) THEN
		  IF ( IMRKOPR(TAG(NODE)) .EQ. 1 ) GOTO 900
		  RETURN
		END IF

C		Now, check whether the appropriate "follow" operator was
C		actually present in the subset.  If there are multiple
C		occurrences, we want the one that most recently precedes
C		the element in question.

		NODFLW = 0
		DO JJ = 1, NTC
		  IF ( ( CTCO(IDXTA,JJ) .EQ. CFLWOPR ) .AND.
     .		      ( INODTCO(IDXTA,JJ) .GE. INODE(LUN) ) .AND.
     .		      ( INODTCO(IDXTA,JJ) .LE. ISC(INODE(LUN)) ) .AND.
     .		      ( INODTCO(IDXTA,JJ) .LT. NODE ) )
     .		    NODFLW = INODTCO(IDXTA,JJ)
		ENDDO
		IF ( NODFLW .EQ. 0 ) THEN
		  IF ( IMRKOPR(TAG(NODE)) .EQ. 1 ) GOTO 901
		  RETURN
		END IF

C		We found an appropriate corresponding "follow" operator,
C		so now we need to look for a bitmap corresponding to
C		this operator.  First, look for a bitmap indicator.

		NODL236 = 0
		NODBMAP = 0
		JJ = 1
		DO WHILE ( ( JJ .LE. NTC ) .AND.
     .		      ( INODTCO(IDXTA,JJ) .GE. INODE(LUN) ) .AND.
     .		      ( INODTCO(IDXTA,JJ) .LE. ISC(INODE(LUN)) ) .AND.
     .			( NODBMAP .EQ. 0 ) )
		  IF ( CTCO(IDXTA,JJ) .EQ. '236000' ) THEN
		    NODL236 = INODTCO(IDXTA,JJ)
		    IF ( INODTCO(IDXTA,JJ) .EQ. NODFLW ) THEN
		      NODBMAP = NODFLW
		    END IF
		  ELSE IF ( ( CTCO(IDXTA,JJ) .EQ. '235000' ) .OR.
     .			    ( CTCO(IDXTA,JJ) .EQ. '237255' ) ) THEN
		    NODL236 = 0
		  ELSE IF ( ( CTCO(IDXTA,JJ) .EQ. '237000' ) .AND.
     .			   ( INODTCO(IDXTA,JJ) .EQ. NODFLW ) .AND.
     .			    ( NODL236 .NE. 0 ) ) THEN
		    NODBMAP = NODL236
		  END IF
		  JJ = JJ + 1
		END DO
		IF ( NODBMAP .EQ. 0 ) THEN

C		  There was no valid bitmap indicator, so we'll just
C		  look for a bitmap after the "follow" indicator.

		  NODBMAP = NODFLW
		END IF

C		Find the corresponding bitmap.

		NN = 1
		IDXBTM = 0
		DO WHILE ( ( IDXBTM .EQ. 0 ) .AND.
     .			  ( NN .LE. NVAL(LUN) ) )
		  IF ( INV( NN, LUN ) .GT. NODBMAP ) THEN
		    II = 1
		    DO WHILE ( ( IDXBTM .EQ. 0 ) .AND.
     .			      ( II .LE. NBTM ) )
		      IF ( NN .EQ. ISTBTM(II) ) THEN
			IDXBTM = II
		      ELSE
			II = II + 1
		      END IF
		    END DO
		  END IF
		  NN = NN + 1
		END DO
		IF ( IDXBTM .EQ. 0 ) THEN
		  IF ( IMRKOPR(TAG(NODE)) .EQ. 1 ) GOTO 902
		  RETURN
		END IF

C		Use the bitmap to find the previous element in the
C		subset that is referenced by the current element.
C		Search backwards from the start of the bitmap, but
C		make sure not to cross a 2-35-000 operator.

		IF ( LSTNODCT .GT. NBTMSE(IDXBTM) ) THEN
		  IF ( IMRKOPR(TAG(NODE)) .EQ. 1 ) GOTO 903
		  RETURN
		END IF
		IEMRK = ISZBTM(IDXBTM) - IBTMSE(IDXBTM,LSTNODCT) + 1
		IECT = 0
		DO WHILE ( ( NN .GE. 1 ) .AND. ( IGETRFEL .EQ. 0 ) )
		  NODNN = INV( NN, LUN )
		  IF ( NODNN .LE. NODBMAP ) THEN
		    DO JJ = 1, NTC
		      IF ( ( NODNN .EQ. INODTCO(IDXTA,JJ) ) .AND.
     .			  ( CTCO(IDXTA,JJ) .EQ. '235000' ) ) THEN
			IF ( IMRKOPR(TAG(NODE)) .EQ. 1 ) GOTO 903
			RETURN
		      END IF
		    END DO
		    IF ( ITP(NODNN) .GT. 1 ) THEN
		      IECT = IECT + 1
		      IF ( IECT .EQ. IEMRK ) IGETRFEL = NN
		    END IF
		  END IF
		  NN = NN - 1
		END DO
		IF ( IGETRFEL .EQ. 0 ) THEN
		  IF ( IMRKOPR(TAG(NODE)) .EQ. 1 ) GOTO 903
		  RETURN
		END IF

		IF ( IMRKOPR(TAG(NODE)) .EQ. 1 ) THEN

C		  This element is a marker operator, so set the scale,
C		  reference value and bit width accordingly based on
C		  those of the previous referenced element.
	
		  NODRFE = INV( IGETRFEL, LUN )
		  ISC(NODE) = ISC(NODRFE)
		  IF ( TAG(NODE)(1:3) .EQ. '225' ) THEN
		    IBT(NODE) = IBT(NODRFE) + 1
		    IRF(NODE) = -1 * (2 ** IBT(NODRFE))
		  ELSE  
		    IBT(NODE) = IBT(NODRFE)
		    IRF(NODE) = IRF(NODRFE)
		    IF ( NNRV .GT. 0 ) THEN
		      DO II = 1, NNRV
			IF ( ( NODRFE .NE. INODNRV(II) ) .AND.
     .			    ( TAG(NODRFE)(1:8) .EQ. TAGNRV(II) ) .AND.
     .			    ( NODRFE .GE. ISNRV(II) ) .AND.
     .			    ( NODRFE .LE. IENRV(II) ) ) THEN
			  IRF(NODE) = NRV(II)
			  RETURN
			END IF
		      END DO
		    END IF
		  END IF
		END IF

	    END IF
	END IF

	RETURN
900	WRITE(BORT_STR,'("BUFRLB: IGETRFEL - UNABLE TO DETERMINE '//
     .	    'FOLLOW OPERATOR FOR MARKER OPERATOR ",A)') TAG(NODE)
	CALL BORT(BORT_STR)
901	WRITE(BORT_STR,'("BUFRLB: IGETRFEL - UNABLE TO FIND FOLLOW '//
     .	    'OPERATOR ",A," IN SUBSET")') CFLWOPR
	CALL BORT(BORT_STR)
902	WRITE(BORT_STR,'("BUFRLB: IGETRFEL - UNABLE TO FIND BITMAP '//
     .	    'FOR MARKER OPERATOR ",A)') TAG(NODE)
	CALL BORT(BORT_STR)
903	WRITE(BORT_STR,'("BUFRLB: IGETRFEL - UNABLE TO FIND PREVIOUS '//
     .	    'ELEMENT REFERENCED BY MARKER OPERATOR ",A)') TAG(NODE)
	CALL BORT(BORT_STR)
	END
