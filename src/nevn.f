C> @file
C> @brief Search for stacked data events within a specified portion
C> of the current data subset.
C>
C> @author J. Woollen @date 2003-11-04

C> This function looks for all stacked data events for a
C> specified data value and level within the portion of the current
C> subset buffer bounded by the indices INV1 and INV2.  All such
C> events are accumulated and returned to the calling program within
C> array USR.  The value of the function itself is the total number
C> of events found.
C>
C> @param[in] NODE - integer: jump/link table index of node for which
C>                   to return stacked values
C> @param[in] LUN  - integer: I/O stream index into internal memory arrays
C> @param[in] INV1 - integer: Starting index of the portion of the subset
C>                   buffer in which to look for stack values
C> @param[in] INV2 - integer: ending index of the portion of the subset
C>                   buffer in which to look for stack values
C> @param[in] I1   - integer: Length of first dimension of USR
C> @param[in] I2   - integer: Length of second dimension of USR
C> @param[in] I3   - integer: Length of third dimension of USR
C> @param[out] USR - real*8(*,*,*): Starting address of data values read
C>                   from data subset; events are returned in the third
C>                   dimension for a particular data value and level in the
C>                   first and second dimensions
C> @returns NEVN   - integer: Number of events in stack (must be less than
C>                   or equal to I3)
C>
C> @note: This routine should only be called by routine ufbin3(),
C> which itself is called only by verification
C> application program gridtobs, where it was previously
C> an in-line subroutine.  In general, nevn() does not work
C> properly in other application programs at this time.
C>
C> @author J. Woollen @date 2003-11-04
      FUNCTION NEVN(NODE,LUN,INV1,INV2,I1,I2,I3,USR)

      USE MODA_USRINT

      CHARACTER*128 BORT_STR
      DIMENSION     USR(I1,I2,I3)
      REAL*8        USR

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      NEVN = 0

C  FIND THE ENCLOSING EVENT STACK DESCRIPTOR
C  -----------------------------------------

      NDRS = LSTJPB(NODE,LUN,'DRS')
      IF(NDRS.LE.0) GOTO 100

      INVN = INVWIN(NDRS,LUN,INV1,INV2)
      IF(INVN.EQ.0) GOTO 900

      NEVN = NINT(VAL(INVN,LUN))
      IF(NEVN.GT.I3) GOTO 901

C  SEARCH EACH STACK LEVEL FOR THE REQUESTED NODE AND COPY THE VALUE
C  -----------------------------------------------------------------

      N2 = INVN + 1

      DO L=1,NEVN
        N1 = N2
        N2 = N2 + NINT(VAL(N1,LUN))
        DO N=N1,N2
        IF(INV(N,LUN).EQ.NODE) USR(1,1,L) = VAL(N,LUN)
        ENDDO
      ENDDO

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: NEVN - CAN''T FIND THE EVENT STACK!!!!!!')
901   WRITE(BORT_STR,'("BUFRLIB: NEVN - THE NO. OF EVENTS FOR THE '//
     . 'REQUESTED STACK (",I3,") EXCEEDS THE VALUE OF THE 3RD DIM. OF'//
     . ' THE USR ARRAY (",I3,")")') NEVN,I3
      CALL BORT(BORT_STR)
      END
