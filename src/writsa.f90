!> @file
!> @brief Write a data subset into a BUFR message, and return each
!> completed message within a memory array.

!> This subroutine is similar to subroutine writsb(), except that in
!> addition to writing each completed message to a specified Fortran
!> logical unit, it also returns a copy of each completed message to
!> the application program within a memory array.
!>
!> @authors J. Woollen
!> @authors J. Ator
!> @date 1994-01-06
!>
!> <b>Usage:</b> call writsa( LUNXX, LMSGT, MSGT, MSGL )
!>
!> @param[in] LUNXX   -- integer: Absolute value is Fortran logical
!>                       unit number for BUFR file
!> @param[in] LMSGT   -- integer: Dimensioned size (in integers) of
!>                       MSGT; used by the subroutine to ensure that
!>                      it doesn't overflow the MSGT array
!> @param[out] MSGT   -- integer(*): BUFR message
!> @param[out] MSGL   -- integer: Size (in integers) of BUFR message
!>                        in MSGT
!>                        - 0 = No BUFR message was returned within
!>                              MSGT
!> 
!> <p>This subroutine looks and behaves a lot like subroutine writsb().
!> Specifically, it is called to indicate to the BUFRLIB software that
!> all necessary values for a data subset (i.e. report) have been written,
!> and thus that the subset is ready to be encoded and packed into the
!> current message for the BUFR file associated with logical unit
!> ABS(LUNXX).  Logical unit ABS(LUNXX) should have already been opened
!> for output operations via a previous call to subroutine openbf(),
!> and a BUFR message should already be open for output within internal
!> arrays via a previous call to one of the BUFRLIB
!> [message-writing subroutines](@ref hierarchy).
!> Furthermore, all of the values for the data subset should have
!> already been written into internal arrays via calls to any of the
!> BUFRLIB [values-writing subroutines](@ref hierarchy)
!>
!> <p>Where this subroutine differs from writsb() is that, in addition
!> to doing all of the above, it also returns a copy of each completed
!> BUFR message to the application program within a memory array.
!> When using this subroutine, it is important to note that the BUFRLIB
!> software is designed to pack as many data subsets as possible into
!> each message for output, and therefore not every call to this
!> subroutine will result in a message being returned in MSGT. In
!> such cases, MSGL will contain the value 0, indicating that no
!> message was returned.
!>
!> <p>In other words, only when MSGL contains a value
!> greater than 0 is there an actual BUFR message within MSGT; otherwise,
!> the message into which the data subset was packed remains internally
!> within BUFRLIB so that future data subsets can be packed into it as
!> well, and the message will eventually be returned during some other
!> future call to this subroutine. For this reason, there is a way to
!> force the subroutine to return any message contained within the
!> internal BUFRLIB arrays, such as when there are no more data subsets
!> to be encoded and we're ready to exit the application program.  In
!> this case, the application program should make one final call to
!> this subroutine, but with LUNXX set to a negative value;
!> specifically, to the additive inverse of the Fortran logical unit
!> number of the BUFR file.  This signals to the subroutine that there
!> are no more data subsets to be packed into the current message for
!> logical unit ABS(LUNXX), and that the existing message should
!> instead be immediately flushed to output and returned in MSGT.
!>
!> @remarks
!> - There is a maximum size for any BUFR message that can be written
!> by the BUFRLIB software.  This maximum message size is initially set
!> to an internal default value within subroutine bfrini(), but it can
!> be changed to a different value via a separate prior call to
!> subroutine maxout().
!> - As is the case for subroutine writsb(), this subroutine will also
!> check to ensure that the data subset, when encoded and packed, will
!> fit into the current BUFR message that is open within the internal
!> arrays associated with logical unit ABS(LUNXX).  If adding the data
!> subset to the current message would cause the maximum message size
!> to be exceeded, then the subroutine will automatically flush the
!> current message to logical unit ABS(LUNXX) and to array MSGT, then
!> open and initialize a new internal message using the same SUBSET and
!> JDATE values that were specified in the most recent call to one of
!> the [message-writing subroutines](@ref hierarchy) for ABS(LUNXX),
!> then encode and pack the data subset into that new message.
!> - If the user would prefer that output messages only be returned
!> to the calling program via the MSGT memory array and not also
!> written to Fortran logical unit ABS(LUNXX), then this can be
!> accomplished by setting IO = 'NUL' when calling subroutine openbf()
!> for ABS(LUNXX).  In such cases, the logical unit number ABS(LUNXX)
!> does not even need to be associated with an actual file on the
!> local system.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1994-01-06 | J. Woollen | Original author |
!> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort(); modified to make Y2K compliant |
!> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
!> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
!> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
!> | 2004-08-18 | J. Ator    | Add post msgupd() check for and return of message within MSGT in order to prevent loss of message in certain situations; maximum message length increased from 20,000 to 50,000 bytes |
!> | 2005-03-09 | J. Ator    | Added capability for compressed messages |
!> | 2009-03-23 | J. Ator    | Added LMSGT argument and check |
!> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
!> | 2019-05-09 | J. Ator    | Added dimensions for MSGLEN and MSGTXT |
!> | 2020-09-22 | J. Ator    | Added capability to return two BUFR messages within MSGT during the same call to this routine, in the rare instances where this can occur |
!> | 2022-02-01 | J. Ator    | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module subroutine_writsa

    private
    public writsa

    interface writsa
        module procedure writsa_4_d, writsa_8
    end interface

    contains

    subroutine writsa_4_d( lunxx, lmsgt, msgt, msgl )
!       used when call arguments to writsa are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: lunxx, lmsgt
        integer(kind=4), intent(out) :: msgt(:), msgl

        integer :: my_lunxx, my_lmsgt, my_msgt(lmsgt), my_msgl

        my_lunxx = lunxx
        my_lmsgt = lmsgt

        call writsa_body( my_lunxx, my_lmsgt, my_msgt, my_msgl )

        msgt(1:lmsgt) = my_msgt(1:lmsgt)
        msgl = my_msgl

    end subroutine writsa_4_d

    subroutine writsa_8( lunxx, lmsgt, msgt, msgl )
!       used when call arguments to writsa are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: lunxx, lmsgt
        integer(kind=8), intent(out) :: msgt(:), msgl

        integer :: my_lunxx, my_lmsgt, my_msgt(lmsgt), my_msgl

        my_lunxx = lunxx
        my_lmsgt = lmsgt

        call writsa_body( my_lunxx, my_lmsgt, my_msgt, my_msgl )

        msgt(1:lmsgt) = my_msgt(1:lmsgt)
        msgl = my_msgl

    end subroutine writsa_8

end module

subroutine writsa_body( lunxx, lmsgt, msgt, msgl )

      USE MODA_BUFRMG
      use subroutine_closmg

      COMMON /MSGCMP/ CCMF

      CHARACTER*1 CCMF

      DIMENSION MSGT(*)

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      LUNIT = ABS(LUNXX)

!  CHECK THE FILE STATUS
!  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

!  IF LUNXX < 0, FORCE MEMORY MSG TO BE WRITTEN (W/O ANY CURRENT SUBSET)
!  ---------------------------------------------------------------------

      IF(LUNXX.LT.0) CALL CLOSMG(LUNIT)

!  IS THERE A COMPLETED BUFR MESSAGE TO BE RETURNED?
!  -------------------------------------------------

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

!  PACK UP THE SUBSET AND PUT IT INTO THE MESSAGE
!  ----------------------------------------------

      CALL WRTREE(LUN)
      IF( CCMF.EQ.'Y' ) THEN
          CALL WRCMPS(LUNIT)
      ELSE
          CALL MSGUPD(LUNIT,LUN)
      ENDIF

!  IF THE JUST-COMPLETED CALL TO WRCMPS OR MSGUPD FOR THIS SUBSET CAUSED
!  A MESSAGE TO BE FLUSHED TO ABS(LUNXX), THEN ATTEMPT TO RETRIEVE AND
!  RETURN THAT MESSAGE NOW.  OTHERWISE, WE RUN THE RISK THAT THE NEXT
!  CALL TO OPENMB OR OPENMG MIGHT CAUSE ANOTHER MESSAGE TO BE FLUSHED,
!  AND THUS OVERWRITE THE CURRENT MESSAGE WITHIN ARRAY MSGTXT BEFORE WE
!  HAD THE CHANCE TO RETRIEVE IT DURING THE NEXT CALL TO WRITSA.

!  ALSO NOTE THAT, IN RARE INSTANCES (E.G. IF THE BYTE COUNT OF THE MOST
!  RECENT SUBSET IS > 65530), WE COULD END UP WITH TWO BUFR MESSAGES
!  AVAILABLE TO BE RETURNED FROM THIS ONE CALL TO WRITSA.  IF SUFFICIENT
!  SPACE IS AVAILABLE IN THE MSGT ARRAY, THEN GO AHEAD AND RETURN BOTH
!  MESSAGES NOW.

      IF( (MSGLEN(LUN).GT.0) .AND. (MSGL+MSGLEN(LUN).LE.LMSGT) ) THEN
         DO N = 1,MSGLEN(LUN)
           MSGT(MSGL+N) = MSGTXT(N,LUN)
         ENDDO
         MSGL = MSGL+MSGLEN(LUN)
         MSGLEN(LUN) = 0
      ENDIF

!  EXITS
!  -----

100   RETURN
900   CALL BORT('BUFRLIB: WRITSA - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: WRITSA - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
902   CALL BORT('BUFRLIB: WRITSA - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE')
904   CALL BORT('BUFRLIB: WRITSA - OVERFLOW OF OUTPUT BUFR MESSAGE ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')

end
