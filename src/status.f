C> @file
C> @brief Check whether a file is connected to the NCEPLIBS-bufr software.
C>
C> @author J. Woollen @date 1994-01-06

C> Check whether a specified Fortran logical unit
C> number is currently connected to the NCEPLIBS-bufr software.
C>
C> If the unit number is already connected, then the subroutine
C> returns information about the associated file.  Otherwise, it
C> returns the next available file ID that could
C> be used to connect the associated file to the software via a
C> subsequent call to subroutine wtstat().
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for
C> BUFR file.
C> @param[out] LUN  - integer: File ID associated with LUNIT.
C> - 0 = LUNIT is not already connected to the software, <b>and</b>
C> there is no remaining internal space available that could be used
C> to connect it.
C> @param[out] IL - integer: File status:
C> - 0 = LUNIT is not already connected to the software, but LUN
C> contains a file ID that could be used to connect it via a subsequent
C> call to subroutine wtstat().
C> - 1 = LUNIT is already connected to the software for output
C> operations (i.e. writing/encoding BUFR).
C> - -1 = LUNIT is already connected to the software for input
C> operations (i.e. reading/decoding BUFR).
C> @param[out] IM  - integer: Message status, indicating whether
C> there is already a message open within internal arrays for LUNIT.
C> - 0 = No
C> - 1 = Yes
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE STATUS(LUNIT,LUN,IL,IM)

      USE MODV_NFILES
      USE MODV_IM8B

      USE MODA_STBFR

      CHARACTER*128 BORT_STR, ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL STATUS(MY_LUNIT,LUN,IL,IM)
         CALL X48(LUN,LUN,1)
         CALL X48(IL,IL,1)
         CALL X48(IM,IM,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IF(LUNIT.LE.0 .OR. LUNIT.GT.99) GOTO 900

      IF ( .NOT. ALLOCATED(IOLUN) ) THEN
        CALL ERRWRT('++++++++++++++++++++WARNING++++++++++++++++++++++')
        ERRSTR = 'BUFRLIB: STATUS WAS CALLED WITHOUT HAVING ' //
     .           'PREVIOUSLY CALLED OPENBF'
        CALL ERRWRT(ERRSTR)
        CALL ERRWRT('++++++++++++++++++++WARNING++++++++++++++++++++++')
        RETURN
      ENDIF

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
