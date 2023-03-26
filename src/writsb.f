C> @file
C> @brief Write a data subset into a BUFR message.
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine writes a complete data subset into a BUFR message,
C> for eventual output to logical unit LUNIT.
C>
C> This subroutine is called when all necessary values for a data subset
C> (i.e. report) have been written, and the subset is ready to be encoded
C> and packed into the current message for the BUFR file associated with
C> logical unit LUNIT. Logical unit LUNIT should have already been
C> opened for output operations via a previous call to subroutine
C> openbf(), and a BUFR message should already be open for output within
C> internal arrays via a previous call to one of the BUFRLIB
C> [message-writing subroutines](@ref hierarchy). All of the values for
C> the data subset should have already been written into internal arrays
C> via calls to any of the BUFRLIB [values-writing subroutines](@ref
C> hierarchy).
C>
C> There is a maximum size for any BUFR message that can be written
C> by the BUFRLIB software. This maximum message size is initially set
C> to an internal default value within subroutine bfrini(), but it can
C> be changed to a different value via a separate prior call to
C> subroutine maxout().
C>
C> This subroutine will always check to ensure that the data subset,
C> when encoded and packed, will fit into the current BUFR message that
C> is already open within the internal arrays associated with logical
C> unit LUNIT. If adding the data subset to the current message would
C> cause the maximum message size to be exceeded, then the subroutine will
C> automatically flush the current message to logical unit LUNIT, then
C> open and initialize a new internal message using the same SUBSET and
C> JDATE values that were specified in the most recent call to one of
C> the [message-writing subroutines](@ref hierarchy) for LUNIT, then
C> encode and pack the data subset into that new message.
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file.
C>
C> @author J. Woollen @date 1994-01-06

      RECURSIVE SUBROUTINE WRITSB(LUNIT)

      USE MODV_IM8B

      COMMON /MSGCMP/ CCMF

      CHARACTER*1 CCMF

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84 ( LUNIT, MY_LUNIT, 1 )
         CALL WRITSB ( MY_LUNIT )

         IM8B=.TRUE.
         RETURN
      ENDIF

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C  PACK UP THE SUBSET AND PUT IT INTO THE MESSAGE
C  ----------------------------------------------

      CALL WRTREE(LUN)
      IF( CCMF.EQ.'Y' ) THEN
          CALL WRCMPS(LUNIT)
      ELSE
          CALL MSGUPD(LUNIT,LUN)
      ENDIF

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: WRITSB - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: WRITSB - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
902   CALL BORT('BUFRLIB: WRITSB - A MESSAGE MUST BE OPEN IN OUTPUT '//
     . 'BUFR FILE, NONE ARE')
      END
