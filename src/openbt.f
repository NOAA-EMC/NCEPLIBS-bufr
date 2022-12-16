C> @file
C> @brief Specify DX BUFR table file of last resort.

C> This subroutine is called as a last resort from within subroutine
C> cktaba(), in the event the latter subroutine is unable to locate
C> a DX BUFR table for use in reading/decoding a given BUFR message.
C>
C> <p>Within the BUFRLIB distribution package, this subroutine
C> is a default placeholder which always returns LUNDX = 0
C> and is only included to allow application programs to compile
C> without generating a link error for an unresolved external
C> reference.  However, users are free to define their own inline
C> version of this subroutine with the same name and calling sequence
C> and include it within the compilation of their application program
C> in order to override the default placeholder version of this
C> subroutine.  In such cases, subroutine cktaba() will then use
C> the inline version of this subroutine as a last resort when
C> attempting to locate the required DX BUFR table file.
C>
C> @author J. Woollen
C> @date 1998-07-08
C>
C> @param[in] MTYP -- integer: Data category of BUFR message for which
C>                    subroutine cktaba() was unable to locate a
C>                    DX BUFR table file
C> @param[out] LUNDX -- integer: Fortran logical unit number for file
C>                      containing DX BUFR table information to be
C>                      used in decoding message
C>                      - 0 = No such file is available
C>
C> @remarks
C> - See [DX BUFR Tables](@ref dfbftab) for more information about
C> the format and contents of DX BUFR table files.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1998-07-08 | J. Woollen | Original author |
C> | 2009-04-21 | J. Ator    | Use errwrt() |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      RECURSIVE SUBROUTINE OPENBT(LUNDX,MTYP)

      USE MODV_IM8B

      COMMON /QUIET / IPRT

      CHARACTER*128 ERRSTR

C     CHECK FOR I8 INTEGERS

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(MTYP,MY_MTYP,1)
         CALL OPENBT(LUNDX,MY_MTYP)
         CALL X48(LUNDX,LUNDX,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: OPENBT - THIS IS A DUMMY BUFRLIB ROUTINE'//
     . ' CALLED BY CKTABA OR APPL. PGM; OPENBT SHOULD BE INCL.'//
     . ' IN-LINE IN APPL. PGM'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

      LUNDX = 0

      RETURN
      END
