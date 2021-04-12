C> @file
C> @brief Check whether a system file is connected to the BUFRLIB
C> software.

C> This subroutine checks whether a specified Fortran logical unit
C> number is currently connected to the BUFRLIB software.
C>
C> <p>If the unit number is already connected, then the subroutine
C> returns information about the associated file.  Otherwise, it
C> returns the next available internal I/O stream index that could
C> be used to connect the associated file to the software via a
C> subsequent call to subroutine wtstat().
C>
C> @author J. Woollen
C> @date 1994-01-06
C>       
C> @param[in]  LUNIT    - integer: Fortran logical unit number for
C>                        BUFR file
C> @param[out]  LUN     - integer: Internal I/O stream index associated
C>                        with LUNIT
C>                        - 0 = LUNIT is not already connected to the
C>                              software, <b>and</b> there is no
C>                              remaining internal space available
C>                              that could be used to connect it
C> @param[out]  IL      - integer: File status
C>                        - 0 = LUNIT is not already connected to the
C>                              software, but LUN contains a new
C>                              internal I/O stream index that could
C>                              be used to connect it via a subsequent
C>                              call to subroutine wtstat()
C>                        - 1 = LUNIT is already connected to the
C>                              software for output operations
C>                              (i.e. writing/encoding BUFR)
C>                        - -1 = LUNIT is already connected to the
C>                               software for input operations
C>                               (i.e. reading/decoding BUFR)
C> @param[out]  IM      - integer: Message status, indicating whether
C>                        there is already a message open within
C>                        internal arrays for LUNIT
C>                        - 0 = No
C>                        - 1 = Yes
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1996-12-11  J. Woollen -- Fixed a long standing bug which occurs in
C>                             unusual situations, very low impact
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine ABORT
C>                             with call to new internal routine bort()
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                             opened at one time increased from 10 to 32
C>                             (necessary in order to process multiple
C>                             BUFR files under the MPI)
C> - 2003-11-04  J. Ator    -- Added documentation
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                             documentation; outputs more complete
C>                             diagnostic info when routine terminates
C>                             abnormally
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>
      SUBROUTINE STATUS(LUNIT,LUN,IL,IM)

      USE MODA_STBFR

      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IF(LUNIT.LE.0 .OR. LUNIT.GT.99) GOTO 900

C  CLEAR THE STATUS INDICATORS
C  ---------------------------

      LUN = 0
      IL  = 0
      IM  = 0

C  SEE IF UNIT IS ALREADY CONNECTED TO BUFR ARCHIVE LIBRARY SOFTWARE
C  -----------------------------------------------------------------

      DO I=1,NFILES
      IF(ABS(IOLUN(I)).EQ.LUNIT) LUN = I
      ENDDO

C  IF NOT, TRY TO DEFINE IT SO AS TO CONNECT IT TO BUFR ARCHIVE LIBRARY
C  SOFTWARE
C  --------------------------------------------------------------------

      IF(LUN.EQ.0) THEN
         DO I=1,NFILES
         IF(IOLUN(I).EQ.0) THEN

C  File space is available, return with LUN > 0, IL and IM remain 0
C  ----------------------------------------------------------------

            LUN = I
            GOTO 100
         ENDIF
         ENDDO

C  File space is NOT available, return with LUN, IL and IM all 0
C  -------------------------------------------------------------

         GOTO 100
      ENDIF

C  IF THE UNIT WAS ALREADY CONNECTED TO THE BUFR ARCHIVE LIBRARY
C   SOFTWARE PRIOR TO THIS CALL, RETURN STATUSES
C  -------------------------------------------------------------

      IL = SIGN(1,IOLUN(LUN))
      IM = IOMSG(LUN)

C  EXITS
C  ----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: STATUS - INPUT UNIT NUMBER (",I3,") '//
     . 'OUTSIDE LEGAL RANGE OF 1-99")') LUNIT
      CALL BORT(BORT_STR)
      END
