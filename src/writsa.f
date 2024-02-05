C> @file
C> @brief Write a data subset into a BUFR message, and return each
C> completed message within a memory array.
C>
C> @author J. Woollen @author J. Ator @date 1994-01-06

C> Write a data subset into a BUFR message, and return each
C> completed message within a memory array.
C>
C> This subroutine is similar to subroutine writsb(), except that in
C> addition to writing each completed message to a specified Fortran
C> logical unit, it also returns a copy of each completed message to
C> the application program within a memory array.
C>
C> This subroutine is called to indicate that
C> all necessary values for a data subset (i.e. report) have been written,
C> and the subset is ready to be encoded and packed into the
C> current message for the BUFR file associated with logical unit
C> ABS(LUNXX). Logical unit ABS(LUNXX) should have already been opened
C> for output operations via a previous call to subroutine openbf(),
C> and a BUFR message should already be open for output within internal
C> arrays via a previous call to one of the NCEPLIBS-bufr
C> [message-writing subroutines](@ref hierarchy).
C> All of the values for the data subset should have
C> already been written into internal arrays via calls to any of the
C> NCEPLIBS-bufr [values-writing subroutines](@ref hierarchy)
C>
C> This subroutine returns a copy of each completed
C> BUFR message to the application program within a memory array.
C> When using this subroutine, it is important to note that the NCEPLIBS-bufr
C> software is designed to pack as many data subsets as possible into
C> each message for output, and therefore not every call to this
C> subroutine will result in a message being returned in MSGT. In
C> such cases, MSGL will contain the value 0, indicating that no
C> message was returned.
C>
C> Only when MSGL contains a value
C> greater than 0 is there an actual BUFR message within MSGT; otherwise,
C> the message into which the data subset was packed remains internally
C> within NCEPLIBS-bufr so that future data subsets can be packed into it as
C> well, and the message will eventually be returned during some other
C> future call to this subroutine. For this reason, there is a way to
C> force the subroutine to return any message contained within the
C> internal NCEPLIBS-bufr arrays, such as when there are no more data subsets
C> to be encoded and we're ready to exit the application program. In
C> this case, the application program should make one final call to
C> this subroutine, but with LUNXX set to a negative value;
C> specifically, to the additive inverse of the Fortran logical unit
C> number of the BUFR file. This signals to the subroutine that there
C> are no more data subsets to be packed into the current message for
C> logical unit ABS(LUNXX), and that the existing message should
C> instead be immediately flushed to output and returned in MSGT.
C>
C> @remarks
C> - There is a maximum size for any BUFR message that can be written
C> by the NCEPLIBS-bufr software. This maximum message size is initially set
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
C> JDATE values that were specified in the most recent call to one of
C> the [message-writing subroutines](@ref hierarchy) for ABS(LUNXX),
C> then encode and pack the data subset into that new message.
C> - If the user would prefer that output messages only be returned
C> to the calling program via the MSGT memory array and not also
C> written to Fortran logical unit ABS(LUNXX), then this can be
C> accomplished by setting IO = 'NUL' when calling subroutine openbf()
C> for ABS(LUNXX). In such cases, the logical unit number ABS(LUNXX)
C> does not even need to be associated with an actual file on the
C> local system.
C>
C> @param[in] LUNXX - integer: Absolute value is Fortran logical unit
C> number for BUFR file.
C> @param[in] LMSGT - integer: Dimensioned size (in integers) of MSGT;
C> used by the subroutine to ensure that it doesn't overflow the MSGT
C> array.
C> @param[out] MSGT - integer: BUFR message.
C> @param[out] MSGL - integer: Size (in integers) of BUFR message in
C> MSGT (0 for no message).
C>
C> @author J. Woollen @author J. Ator @date 1994-01-06

      RECURSIVE SUBROUTINE WRITSA(LUNXX,LMSGT,MSGT,MSGL)

      use modv_vars, only: im8b

      use moda_bufrmg

      COMMON /MSGCMP/ CCMF

      CHARACTER*1 CCMF

      DIMENSION MSGT(*)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84 ( LUNXX, MY_LUNXX, 1 )
         CALL X84 ( LMSGT, MY_LMSGT, 1 )
         CALL WRITSA ( MY_LUNXX, MY_LMSGT*2, MSGT, MSGL )
         MSGL = MSGL/2
         CALL X48 ( MSGL, MSGL, 1 )

         IM8B=.TRUE.
         RETURN
      ENDIF

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
