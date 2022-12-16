C> @file
C> @brief Write a long character string (greater than 8 bytes) to
C> a data subset

C> This subroutine writes a long character string (greater than 8 bytes)
C> to a data subset.
C>
C> <p>Normally, subroutine writlc() is used to write a long character
C> string to a data subset.  However, subroutine writlc() can only be
C> called <b>after</b> a call to one of the
C> [subset-writing subroutines](@ref hierarchy), so it will not work
C> for cases when one of those subroutines flushes the message
C> containing the data subset in question to logical unit LUNIT during
C> the same call to that subroutine, such as when the data subset
C> contains more than 65530 bytes.  When this happens, there is no
C> longer any way for a subsequent writlc() call to write a long
C> character string into that data subset, because the data subset has
C> already been flushed from internal memory.  This subroutine solves
C> that problem, by allowing a long character string to be specified
C> <b>before</b> calling one of the
C> [subset-writing subroutines](@ref hierarchy), and the string value
C> will be held and stored automatically (via an internal call to
C> subroutine writlc()) at the proper time during the subsequent call
C> to the [subset-writing subroutines](@ref hierarchy).
C>
C> @author J. Ator
C> @date 2014-02-05
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR file
C> @param[in] CHR  -- character*(*): Value corresponding to STR
C> @param[in] STR   -- character*(*): Table B mnemonic of long character
C>                     string to be written, possibly supplemented
C>                     with an ordinal occurrence notation
C>
C> <p>If there is more than one occurrence of STR within the data subset
C> definition, then each occurrence can be written via a separate call
C> to this subroutine, and by appending the ordinal number of the
C> occurrence to STR in each case.  For example, if there are 5
C> occurrences of mnemonic LSTID within a given data subset definition,
C> then 5 separate calls should be made to this subroutine, once each
C> with STR set to 'LSTID#1', 'LSTID#2', 'LSTID#3', 'LSTID#4' and
C> 'LSTID#5'.  However, the first notation is superfluous, because
C> omitting the ordinal number always defaults to the first occurrence
C> of a particular string, so a user could just specify 'LSTID'
C> instead of 'LSTID#1'.
C>
C> @remarks
C> - Character strings which are 8 bytes or less in length can be
C> written by converting the string into a real*8 value within the
C> application program, and then using the real*8 USR array within a
C> call to one of the BUFRLIB
C> [values-writing subroutines](@ref hierarchy)
C> prior to calling one of the
C> [subset-writing subroutines](@ref hierarchy)
C> for the data subset.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2014-02-05 | J. Ator | Original author |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      RECURSIVE SUBROUTINE HOLD4WLC(LUNIT,CHR,STR)

      USE MODA_H4WLC
      USE MODV_IM8B

      COMMON /QUIET/ IPRT

      CHARACTER*(*) CHR,STR

      CHARACTER*128 ERRSTR
      CHARACTER*14  MYSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     CHECK FOR I8 INTEGERS

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL HOLD4WLC(MY_LUNIT,CHR,STR)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL STRSUC( STR, MYSTR, LENS )
      IF ( LENS .EQ. -1 ) RETURN

      LENC = MIN( LEN( CHR ), 120 )

C     IF THIS SUBROUTINE HAS ALREADY BEEN CALLED WITH THIS MNEMONIC FOR
C     THIS PARTICULAR SUBSET, THEN OVERWRITE THE CORRESPONDING ENTRY IN
C     THE INTERNAL HOLDING AREA.

      IF ( NH4WLC .GT. 0 ) THEN
        DO I = 1, NH4WLC
          IF ( ( LUNIT .EQ. LUH4WLC(I) ) .AND.
     .         ( MYSTR(1:LENS) .EQ. STH4WLC(I)(1:LENS) ) ) THEN
            CHH4WLC(I) = ''
            CHH4WLC(I)(1:LENC) = CHR(1:LENC)
            RETURN
          ENDIF
        ENDDO
      ENDIF

C     OTHERWISE, USE THE NEXT AVAILABLE UNUSED ENTRY IN THE HOLDING AREA.

      IF ( NH4WLC .GE. MXH4WLC ) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,A,I3)' )
     . 'BUFRLIB: HOLD4WLC - THE MAXIMUM NUMBER OF LONG CHARACTER ',
     . 'STRINGS THAT CAN BE HELD INTERNALLY IS ', MXH4WLC
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
         ENDIF
      ELSE
         NH4WLC = NH4WLC + 1
         LUH4WLC(NH4WLC) = LUNIT
         STH4WLC(NH4WLC) = ''
         STH4WLC(NH4WLC)(1:LENS) = MYSTR(1:LENS)
         CHH4WLC(NH4WLC) = ''
         CHH4WLC(NH4WLC)(1:LENC) = CHR(1:LENC)
      ENDIF

      RETURN
      END
