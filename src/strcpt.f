C> @file
C> @brief Specify a tank receipt time to be included in Section 1
C> when writing BUFR messages.

C> This subroutine is used to specify a tank receipt time to be
C> included within Section 1 of all BUFR messages output by future calls
C> to [message-writing subroutines](@ref hierarchy) and
C> [subset-writing subroutines](@ref hierarchy).
C>
C> @author J. Ator
C> @date 2009-03-23
C>
C> @param[in] CF   -- character*1: Flag indicating whether future BUFR
C>                    output messages should include the tank receipt
C>                    time defined by IYR, IMO, IDY, IHR, and IMI:
C>                     - 'N' = No (the default)
C>                     - 'Y' = Yes
C> @param[in] IYR  -- integer: Tank receipt year
C> @param[in] IMO  -- integer: Tank receipt month
C> @param[in] IDY  -- integer: Tank receipt day
C> @param[in] IHR  -- integer: Tank receipt hour
C> @param[in] IMI  -- integer: Tank receipt minute
C>
C> <p>This subroutine can be called at any time after the first call
C> to subroutine openbf(), and the specified value for CF will remain
C> in effect for all future calls to
C> [message-writing subroutines](@ref hierarchy) and
C> [subset-writing subroutines](@ref hierarchy), for all Fortran logical
C> units that are open for output within the application program,
C> unless a subsequent call is made to this subroutine to reset the
C> value of CF again.  If this subroutine is never called, a default
C> value of 'N' is used for CF, as set within subroutine bfrini().
C>
C> <p>Whenever this subroutine is called with CF = 'N', the values in
C> IYR, IMO, IDY, IHR, and IMI are ignored.
C>
C> @remarks
C> - Tank receipt time is an NCEP extension to Section 1 of the
C> [official WMO BUFR regulations](@ref manual).
C> However, it's encoded by the BUFRLIB software in such a way that
C> its inclusion within an output BUFR message is still fully
C> compliant with the regulations.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2009-03-23 | J. Ator | Original author |
C> | 2022-10-04 | J. Ator | Added 8-byte wrapper |

      RECURSIVE SUBROUTINE STRCPT(CF,IYR,IMO,IDY,IHR,IMI)

      USE MODV_IM8B

      COMMON /TNKRCP/ ITRYR,ITRMO,ITRDY,ITRHR,ITRMI,CTRT

      CHARACTER*128 BORT_STR
      CHARACTER*1   CTRT, CF

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(IYR,MY_IYR,1)
         CALL X84(IMO,MY_IMO,1)
         CALL X84(IDY,MY_IDY,1)
         CALL X84(IHR,MY_IHR,1)
         CALL X84(IMI,MY_IMI,1)
         CALL STRCPT(CF,MY_IYR,MY_IMO,MY_IDY,MY_IHR,MY_IMI)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL CAPIT(CF)
      IF(CF.NE.'Y'.AND. CF.NE.'N') GOTO 900

      CTRT = CF
      IF(CTRT.EQ.'Y') THEN
        ITRYR = IYR
        ITRMO = IMO
        ITRDY = IDY
        ITRHR = IHR
        ITRMI = IMI
      ENDIF

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: STRCPT - INPUT ARGUMENT IS ",A1,'//
     . '", IT MUST BE EITHER Y OR N")') CF
      CALL BORT(BORT_STR)
      END
