C> @file
C> @brief Write a data subset into a BUFR message, and return each
C> completed message within a memory array.

C> This subroutine is similar to subroutine writsb(), except that in
C> addition to writing each completed message to a specified Fortran
C> logical unit, it also returns a copy of each completed message to
C> the application program within a memory array.
C>
C> @authors J. Woollen
C> @authors J. Ator
C> @date 1994-01-06
C>
C> @param[in] LUNXX    - integer: Absolute value is Fortran logical
C>                       unit number for BUFR file
C> @param[in] LMSGT    - integer: Dimensioned size (in integers) of
C>                       MSGT; used by the subroutine to ensure that
C>                      it doesn't overflow the MSGT array
C> @param[out] MSGT    - integer(*): BUFR message
C> @param[out] MSGL    - integer: Size (in integers) of BUFR message
C>                        in MSGT
C>                        - 0 = No BUFR message was returned within
C>                              MSGT
C> 
C> <p>This subroutine looks and behaves a lot like subroutine writsb().
C> Specifically, it is called to indicate to the BUFRLIB software that
C> all necessary values for a data subset (i.e. report) have been written,
C> and thus that the subset is ready to be encoded and packed into the
C> current message for the BUFR file associated with logical unit
C> ABS(LUNXX).  Logical unit ABS(LUNXX) should have already been opened
C> for output operations via a previous call to subroutine openbf(),
C> and a BUFR message should already be open for output within internal
C> arrays via a previous call to subroutine openmg() or openmb().
C> Furthermore, all of the values for the data subset should have
C> already been written into internal arrays via calls to subroutines
C> ufbint(), ufbrep(), ufbseq(), etc.
C>
C> <p>Where this subroutine differs from writsb() is that, in addition
C> to doing all of the above, it also returns a copy of each completed
C> BUFR message to the application program within a memory array.
C> When using this subroutine, it is important to note that the BUFRLIB
C> software is designed to pack as many data subsets as possible into
C> each message for output, and therefore not every call to this
C> subroutine will result in a message being returned in MSGT. In
C> such cases, MSGL will contain the value 0, indicating that no
C> message was returned.
C>
C> <p>In other words, only when MSGL contains a value
C> greater than 0 is there an actual BUFR message within MSGT; otherwise,
C> the message into which the data subset was packed remains internally
C> within BUFRLIB so that future data subsets can be packed into it as
C> well, and the message will eventually be returned during some other
C> future call to this subroutine. For this reason, there is a way to
C> force the subroutine to return any message contained within the
C> internal BUFRLIB arrays, such as when there are no more data subsets
C> to be encoded and we're ready to exit the application program.  In
C> this case, the application program should make one final call to
C> this subroutine, but with LUNXX set to a negative value;
C> specifically, to the additive inverse of the Fortran logical unit
C> number of the BUFR file.  This signals to the subroutine that there
C> are no more data subsets to be packed into the current message for
C> logical unit ABS(LUNXX), and that the existing message should
C> instead be immediately flushed to output and returned in MSGT.
C>
C> @remarks
C> - There is a maximum size for any BUFR message that can be written
C> by the BUFRLIB software.  This maximum message size is initially set
C> to an internal default value within subroutine bfrini(), but it can
C> be changed to a different value via a separate prior call to
C> subroutine maxout().
C> - As is the case for subroutine writsb(), this subroutine will also
C> check to ensure that the data subset, when encoded and packed, will
C> fit into the current BUFR message that is open within the internal
C> arrays associated with logical unit ABS(LUNXX).  If adding the data
C> subset to the current message would cause the maximum message size
C> to be exceeded, then the subroutine will automatically flush the
C> current message to logical unit ABS(LUNXX) and to array MSGT, then
C> open and initialize a new internal message using the same SUBSET and
C> JDATE values that were specified in the most recent call to
C> openmg() or openmb() for ABS(LUNXX), then encode and pack the data
C> subset into that new message.
C> - If the user would prefer that output messages only be returned
C> to the calling program via the MSGT memory array and not also
C> written to Fortran logical unit ABS(LUNXX), then this can be
C> accomplished by setting IO = 'NUL' when calling subroutine openbf()
C> for ABS(LUNXX).  In such cases, the logical unit number ABS(LUNXX)
C> does not even need to be associated with an actual file on the
C> local system.
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine
C>                           "ABORT" with call to new internal BUFRLIB
C>                           routine "BORT"; modified to make Y2K
C>                           compliant
C> - 2000-09-19  J. Woollen -- Maximum message length increased
C>                             from 10,000 to 20,000 bytes
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                           documentation; outputs more complete
C>                           diagnostic info when routine terminates
C>                           abnormally
C> - 2004-08-18  J. Ator    -- Add post msgupd() check for and return of
C>                           message within MSGT in order to prevent
C>                           loss of message in certain situations;
C>                           maximum message length increased from
C>                           20,000 to 50,000 bytes
C> - 2005-03-09  J. Ator    -- Added capability for compressed messages
C> - 2009-03-23  J. Ator    -- Added LMSGT argument and check
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C> - 2019-05-09  J. Ator    -- Added dimensions for MSGLEN and MSGTXT
C> - 2020-09-22  J. Ator    -- Added capability to return two BUFR
C>                           messages within MSGT during the same call
C>                           to this routine, in the rare instances
C>                           where this can occur
C>
      SUBROUTINE WRITSA(LUNXX,LMSGT,MSGT,MSGL)

      USE MODA_BUFRMG

      COMMON /MSGCMP/ CCMF

      CHARACTER*1 CCMF

      DIMENSION MSGT(*)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      LUNIT = ABS(LUNXX)

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C  IF LUNXX < 0, FORCE MEMORY MSG TO BE WRITTEN (W/O ANY CURRENT SUBSET)
C  ---------------------------------------------------------------------

      IF(LUNXX.LT.0) CALL CLOSMG(LUNIT)

C  IS THERE A COMPLETED BUFR MESSAGE TO BE RETURNED?
C  -------------------------------------------------

      IF(MSGLEN(LUN).GT.0) THEN
         IF(MSGLEN(LUN).GT.LMSGT) GOTO 904
         MSGL = MSGLEN(LUN)
         DO N=1,MSGL
           MSGT(N) = MSGTXT(N,LUN)
         ENDDO
         MSGLEN(LUN) = 0
      ELSE
         MSGL = 0
      ENDIF

      IF(LUNXX.LT.0) GOTO 100

C  PACK UP THE SUBSET AND PUT IT INTO THE MESSAGE
C  ----------------------------------------------

      CALL WRTREE(LUN)
      IF( CCMF.EQ.'Y' ) THEN
          CALL WRCMPS(LUNIT)
      ELSE
          CALL MSGUPD(LUNIT,LUN)
      ENDIF

C  IF THE JUST-COMPLETED CALL TO WRCMPS OR MSGUPD FOR THIS SUBSET CAUSED
C  A MESSAGE TO BE FLUSHED TO ABS(LUNXX), THEN ATTEMPT TO RETRIEVE AND
C  RETURN THAT MESSAGE NOW.  OTHERWISE, WE RUN THE RISK THAT THE NEXT
C  CALL TO OPENMB OR OPENMG MIGHT CAUSE ANOTHER MESSAGE TO BE FLUSHED,
C  AND THUS OVERWRITE THE CURRENT MESSAGE WITHIN ARRAY MSGTXT BEFORE WE
C  HAD THE CHANCE TO RETRIEVE IT DURING THE NEXT CALL TO WRITSA.

C  ALSO NOTE THAT, IN RARE INSTANCES (E.G. IF THE BYTE COUNT OF THE MOST
C  RECENT SUBSET IS > 65530), WE COULD END UP WITH TWO BUFR MESSAGES
C  AVAILABLE TO BE RETURNED FROM THIS ONE CALL TO WRITSA.  IF SUFFICIENT
C  SPACE IS AVAILABLE IN THE MSGT ARRAY, THEN GO AHEAD AND RETURN BOTH
C  MESSAGES NOW.

      IF( (MSGLEN(LUN).GT.0) .AND. (MSGL+MSGLEN(LUN).LE.LMSGT) ) THEN
         DO N = 1,MSGLEN(LUN)
           MSGT(MSGL+N) = MSGTXT(N,LUN)
         ENDDO
         MSGL = MSGL+MSGLEN(LUN)
         MSGLEN(LUN) = 0
      ENDIF

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: WRITSA - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: WRITSA - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
902   CALL BORT('BUFRLIB: WRITSA - A MESSAGE MUST BE OPEN IN OUTPUT '//
     . 'BUFR FILE, NONE ARE')
904   CALL BORT('BUFRLIB: WRITSA - OVERFLOW OF OUTPUT BUFR MESSAGE '//
     . 'ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')
      END
