C> @file
C> @brief Get information about a BUFR message

C> This subroutine reads through a BUFR file (starting from the beginning
C> of the file) and returns the message type (from Section 1) of the
C> first message it encounters which does not contain DX BUFR table
C> information.
C>
C> <p>The BUFR file should not have already been opened via a call
C> to subroutine openbf(); however, it should already be associated
C> with Fortran logical unit number LUNIT.
C>
C> <p>This subroutine is similar to subroutine mesgbc(), except that
C> this subroutine will only skip past DX BUFR table messages at the
C> beginning of a file, whereas mesgbc() will also skip past any "dummy"
C> messages containing the dump center time or dump initiation time
C> within NCEP dump files.  Furthermore, mesgbc() also returns a
C> message compression indicator, and it also has an option to operate
C> on a BUFR message that has already been read into the internal arrays.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR file
C> @param[out] MESGTYP -- integer: Message type
C>                        -1 = error reading the BUFR file, or no
C>                             messages were read from the file
C>                        11 = BUFR file only contained DX BUFR table
C>                             messages
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
C> | 2004-08-09 | J. Ator   | Maximum message length increased from 20,000 to 50,000 bytes |
C> | 2005-11-29 | J. Ator   | Use iupbs01() and rdmsgw() |
C> | 2009-03-23 | J. Ator   | Use idxmsg() |
C> | 2012-09-15 | J. Woollen | Convert to C language I/O interface; use 'INX' option with openbf() |
C> | 2013-01-25 | J. Woollen | Always call closbf() before exiting |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      SUBROUTINE MESGBF(LUNIT,MESGTYP)

      USE MODA_MGWA
      USE MODV_IM8B

      INTEGER*8 LUNIT_8,MESGTYP_8

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         LUNIT_8=LUNIT
         CALL MESGBF_8(LUNIT_8,MESGTYP_8)
         MESGTYP=MESGTYP_8

         IM8B=.TRUE.
         RETURN
      ENDIF

      MESGTYP = -1

C  SINCE OPENBF HAS NOT YET BEEN CALLED, CALL IT 
C  ---------------------------------------------

      CALL OPENBF(LUNIT,'INX',LUNIT)

C  READ PAST ANY BUFR TABLES AND RETURN THE FIRST MESSAGE TYPE FOUND
C  -----------------------------------------------------------------

1     CALL RDMSGW(LUNIT,MGWA,IER)
      IF(IER.EQ.0) THEN
         MESGTYP = IUPBS01(MGWA,'MTYP')
         IF(IDXMSG(MGWA).EQ.1) GOTO 1
      ENDIF

C  CLOSE THE FILE
C  --------------
  
      CALL CLOSBF(LUNIT)

C  EXIT
C  ----

100   RETURN
      END

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine mesgbf().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine mesgbf() directly.
C>
C> @author J. Woollen
C> @date 2022-08-04
C>
C> @param[in] LUNIT_8 -- integer*8: Fortran logical unit number for
C>                       BUFR file
C> @param[out] MESGTYP_8 -- integer*8: Message type
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-08-04 | J. Woollen | Original author      |

      SUBROUTINE MESGBF_8(LUNIT_8,MESGTYP_8)

      INTEGER*8 LUNIT_8,MESGTYP_8

      LUNIT=LUNIT_8
      CALL MESGBF(LUNIT,MESGTYP)
      MESGTYP_8=MESGTYP

      RETURN
      END
