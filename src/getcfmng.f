C> @file
C> @brief Decode the meaning of a numerical value from a code or flag table

C> This subroutine searches for a specified Table B mnemonic and associated
C> value (code figure or bit number) within the master Code/Flag tables,
C> and if found returns the associated meaning as a character string.
C>
C> @author J. Ator
C> @date 2018-01-11
C>
C> @param[in]  LUNIT   -- integer: Fortran logical unit number for
C>                        BUFR file
C> @param[in]  NEMOI   -- character*(*): Mnemonic to search for
C> @param[in]  IVALI   -- integer: Value (code figure or bit number)
C>                        associated with NEMOI
C> @param[in]  NEMOD   -- character*(*): Optional second mnemonic upon
C>                        which the values NEMOI and IVALI depend; set to
C>                        all blank characters if the meanings of NEMOI and
C>                        IVALI do not depend on the value of any other
C>                        mnemonic
C> @param[in]  IVALD   -- integer: Value (code figure or bit number)
C>                        associated with NEMOD; set to (-1) whenever
C>                        NEMOD is set to all blank characters
C> @param[out] CMEANG  -- character*(*): If the initial search of the
C>                        master Code/Flag tables was successful, then this
C>                        string contains the meaning corresponding to NEMOI
C>                        and IVALI (and to NEMOD and IVALD, if specified).
C>                        However, if the initial search was unsuccessful,
C>                        <b>and</b> if no optional second mnemonic and
C>                        associated value were specified on input,
C>                        <b>and</b> if a second search of the table
C>                        determines that the meaning of NEMOI and IVALI
C>                        indeed depends on one or more other possible
C>                        second mnemonics, then those possible second
C.                        mnemonics are returned within this string, as a
C>                        series of IRET successive 8-byte substrings. 
C>                        An example of this scenario is included below
C>                        within the Remarks.
C> @param[out] LNMNG   -- integer: Length (in bytes) of string returned in
C>                        CMEANG
C> @param[out] IRET    -- integer: return code
C>                        -  0 = meaning found and stored in CMEANG string
C>                        - -1 = meaning not found
C>                        - >0 = meaning not found, <b>and</b> NEMOD and
C>                               IVALD were not specified on input,
C>                               <b>and</b> the meaning of NEMOI and IVALI
C>                               depends on the value of one of the
C>                               mnemonics stored in the first IRET 8-byte
C>                               substrings of CMEANG
C>
C> <p>As noted above, this subroutine first does an initial search of
C> the master Code/Flag tables based on the mnemonics and values provided.
C> The input parameters NEMOI and IVALI specify the mnemonic and
C> corresponding numerical code or flag table value for which the meaning
C> is sought, and the optional secondary parameters NEMOD and IVALD are
C> specified when needed to differentiate between multiple possible
C> results. An example of this particular scenario is included below
C> within the Remarks.  Otherwise, if the meaning of NEMOD and IVALD
C> does not depend on the value associated with any other mnemonic, then
C> NEMOD should be set to a field of all blank characters, and IVALD
C> should be set to a value of (-1).
C>
C> <p>Subroutine codflg() must be called with a CF value of 'Y' prior to
C> calling this subroutine, in order to ensure that master Code/Flag
C> tables have been read into internal memory.
C>
C> <p>This subroutine can be called at any time after a BUFR message
C> has been read into internal arrays by one of the BUFRLIB
C> [message-reading subroutines](@ref hierarchy), and it
C> can be called for any code or flag table mnemonic defined within that
C> particular message.  In most cases, this means that the mnemonic must
C> be contained within the subset definition (Section 3) of that message.
C> The only exceptions to this rule are for originating centers,
C> originating subcenters, data types and data subtypes, since those can
C> also be contained within the identification section (Section 1) of a
C> BUFR message.
C>
C> <p>It is the user's responsibility to provide sufficient allocated
C> space in CMEANG for the returned meaning string; otherwise, the
C> returned string will be truncated.
C>
C> @remarks
C> - An example of when secondary mnemonics NEMOD and IVALD would be
C> required is when a user is searching for the meaning of a numerical
C> code table value for an originating sub-center (i.e. mnemonic GSES).
C> The meaning of any originating sub-center value depends on the identity
C> of the originating center for which the sub-center in question is a
C> member, so in order for the subroutine to locate and return the proper
C> one, information about the originating center must also be provided. So
C> in this case the user would input GSES and the associated numerical
C> value as NEMOI and IVALI, respectively, but the user would also need to
C> specify an appropriate originating center mnemonic (e.g. GCLONG, OGCE
C> or ORIGC) and associated value from the same BUFR message as input
C> parameters NEMOD and IVALD, respectively, and then the subroutine will
C> be able to locate and return the appropriate meaning string. Otherwise,
C> if this information was not provided, the subroutine would return with
C> an IRET value of 3, and with each of the mnemonics GCLONG, OGCE and
C> ORIGC contained in successive 8-byte substrings of CMEANG (and with a
C> corresponding value of 24 returned for LNMNG), as a hint to the user
C> that more information needs to be input to the subroutine in order to
C> achieve the desired result.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2018-01-11 | J. Ator | Original author |
C> | 2018-02-08 | J. Ator | Add special handling for data types and subtypes in Section 1 |
C>
	SUBROUTINE GETCFMNG ( LUNIT, NEMOI, IVALI, NEMOD, IVALD,
     .			      CMEANG, LNMNG, IRET )

	USE MODA_TABABD

	COMMON /TABLEF/ CDMF

	CHARACTER*(*)	NEMOI, NEMOD, CMEANG

	CHARACTER*128	BORT_STR
	CHARACTER*8	NEMO
	CHARACTER*1	CDMF, TAB

	DIMENSION	IFXYD(10)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	CALL STATUS ( LUNIT, LUN, IL, IM )
	IF ( IL .EQ. 0 ) GOTO 900
	IF ( IL .GT. 0 ) GOTO 901
	IF ( IM .EQ. 0 ) GOTO 902

C*	Make sure the appropriate code/flag information has already been
C*	read into internal memory.

	IF ( CDMF .NE. 'Y' ) GOTO 903

	ITMP = IREADMT ( LUN )

C*	Check the validity of the input mnemonic(s).  Include special
C*	handling for originating centers, originating subcenters, data
C*	types and data subtypes, since those can be reported in
C*	Section 1 of a BUFR message as well as in Section 3, so if a
C*	user requests those mnemonics we can't necessarily assume they
C*	came from within Section 3.

	LCMG = LEN ( CMEANG )

	IF ( NEMOI(1:4) .EQ. 'GSES' ) THEN
	    IF ( ( NEMOD(1:6) .EQ. 'GCLONG' ) .OR.
     .		 ( NEMOD(1:4) .EQ. 'OGCE' ) .OR.
     .		 ( NEMOD(1:5) .EQ. 'ORIGC' ) )  THEN
		IFXYI = IFXY ( '001034' )
		IFXYD(1) = IFXY ( '001035' )
	    ELSE
		LNMNG = MIN ( 24, LCMG )
		IF ( LNMNG .EQ. 24 ) THEN
		    IRET = 3
		    CMEANG(1:24) = 'GCLONG  OGCE    ORIGC   '
		ELSE
		    IRET = -1
		END IF
		RETURN
	    END IF
	ELSE IF ( NEMOI(1:6) .EQ. 'GCLONG' ) THEN
	    IFXYI = IFXY ( '001031' )
	    IFXYD(1) = (-1)
	ELSE IF ( NEMOI(1:4) .EQ. 'OGCE' ) THEN
	    IFXYI = IFXY ( '001033' )
	    IFXYD(1) = (-1)
	ELSE IF ( NEMOI(1:5) .EQ. 'ORIGC' ) THEN
	    IFXYI = IFXY ( '001035' )
	    IFXYD(1) = (-1)
	ELSE IF ( ( NEMOI(1:7) .EQ. 'TABLASS' ) .OR.
     +		  ( NEMOI(1:7) .EQ. 'TABLASL' ) ) THEN
	    IF ( ( NEMOD(1:6) .EQ. 'TABLAT' ) ) THEN
		IF ( NEMOI(1:7) .EQ. 'TABLASS' ) THEN
		    IFXYI = IFXY ( '055021' )
		ELSE
		    IFXYI = IFXY ( '055022' )
		ENDIF
		IFXYD(1) = IFXY ( '055020' )
	    ELSE
		LNMNG = MIN ( 8, LCMG )
		IF ( LNMNG .EQ. 8 ) THEN
		    IRET = 1
		    CMEANG(1:8) = 'TABLAT  '
		ELSE
		    IRET = -1
		END IF
		RETURN
	    END IF
	ELSE IF ( NEMOI(1:6) .EQ. 'TABLAT' ) THEN
	    IFXYI = IFXY ( '055020' )
	    IFXYD(1) = (-1)
	ELSE
	    CALL PARSTR ( NEMOI, NEMO, 1, NTG, ' ', .TRUE. )
	    CALL NEMTAB ( LUN, NEMO, IFXYI, TAB, N )
	    IF ( ( N .EQ. 0 ) .OR. ( TAB .NE. 'B' ) ) GOTO 904
	    IF ( ( TABB ( N, LUN )(71:74) .NE. 'CODE' ) .AND.
     .		 ( TABB ( N, LUN )(71:74) .NE. 'FLAG' ) ) GOTO 905
	    IF ( NEMOD(1:1) .NE. ' ' ) THEN
		CALL PARSTR ( NEMOD, NEMO, 1, NTG, ' ', .TRUE. )
		CALL NEMTAB ( LUN, NEMO, IFXYD(1), TAB, N )
		IF ( ( N .EQ. 0 ) .OR. ( TAB .NE. 'B' ) ) GOTO 904
		IF ( ( TABB ( N, LUN )(71:74) .NE. 'CODE' ) .AND.
     .		     ( TABB ( N, LUN )(71:74) .NE. 'FLAG' ) ) GOTO 905
	    ELSE
	        IFXYD(1) = (-1)
	    END IF
	END IF

C*	Search the internal table for the requested meaning.

	CALL SRCHTBF ( IFXYI, IVALI, IFXYD, 10, IVALD,
     .		       CMEANG, LCMG, LNMNG, IRET )
	IF ( IRET .LE. 0 ) RETURN

C*	The meaning of this value is dependent on the value of another
C*	mnemonic in the report.

	IRET2 = IRET
	LNMNG = 0
	IRET = 0
	DO II = 1, IRET2
	    CALL NUMTBD ( LUN, IFXYD(II), NEMO, TAB, IERBD )
	    IF ( ( IERBD .GT. 0 ) .AND. ( TAB .EQ. 'B' ) .AND.
     .		   ( LCMG .GE. ( LNMNG + 8 ) ) ) THEN
		IRET = IRET + 1
		CMEANG(LNMNG+1:LNMNG+8) = NEMO
		LNMNG = LNMNG + 8
	    END IF
	END DO
	IF ( IRET .EQ. 0 ) IRET = -1

	RETURN
900     CALL BORT('BUFRLIB: GETCFMNG - INPUT BUFR FILE IS CLOSED, IT '//
     .   'MUST BE OPEN FOR INPUT')
901     CALL BORT('BUFRLIB: GETCFMNG - INPUT BUFR FILE IS OPEN FOR '//
     .   'OUTPUT, IT MUST BE OPEN FOR INPUT')
902     CALL BORT('BUFRLIB: GETCFMNG - A MESSAGE MUST BE OPEN IN '//
     .   'INPUT BUFR FILE, NONE ARE')
903     CALL BORT('BUFRLIB: GETCFMNG - TO USE THIS SUBROUTINE, MUST '//
     .   'FIRST CALL SUBROUTINE CODFLG WITH INPUT ARGUMENT SET TO Y')
904	WRITE(BORT_STR,'("BUFRLIB: GETCFMNG - MNEMONIC ",A,'//
     .   '" NOT FOUND IN TABLE B")') NEMO
	CALL BORT(BORT_STR)
905     WRITE(BORT_STR,'("BUFRLIB: GETCFMNG - MNEMONIC ",A,'//
     .   '" IS NOT A CODE OR FLAG TABLE")') NEMO
	CALL BORT(BORT_STR)
	END
