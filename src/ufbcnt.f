C> @file
C> @brief Get the current message number and data subset number within
C> a BUFR file

C> This subroutine returns the current location of the file pointer
C> within a BUFR file, in terms of a message number counting from the
C> beginning of the file, and a data subset number counting from the
C> beginning of that message.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR file
C> @param[out] KMSG -- integer: Ordinal number of current message,
C>                     counting from the beginning of the BUFR file, but
C>                     not counting any messages which contain DX BUFR
C>                     tables information
C> @param[out] KSUB -- integer: Ordinal number of current data subset
C>                     within (KMSG)th message, counting from the
C>                     beginning of the message
C>
C> @remarks
C> - Logical unit LUNIT should have already been opened via a previous
C> call to subroutine openbf().  If LUNIT was opened for input
C> operations, then KMSG is incremented with each call to any of the
C> [message-reading subroutines](@ref hierarchy), and KSUB is
C> incremented with each call to any of the
C> [subset-reading subroutines](@ref hierarchy) for that message.
C> Otherwise, if LUNIT was opened for output operations, then KMSG is
C> incremented with each call to any of the
C> [message-writing subroutines](@ref hierarchy), and KSUB is
C> incremented with each call to any of the
C> [subset-writing subroutines](@ref hierarchy) for that message.
C> - The value returned for KMSG does <b>not</b> include any messages
C> which contain DX BUFR tables information.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
C> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2022-10-04 | J. Ator    | Added 8-byte wrapper |

      SUBROUTINE UFBCNT(LUNIT,KMSG,KSUB)

      USE MODA_MSGCWD
      USE MODV_IM8B

      INTEGER*8 LUNIT_8, KMSG_8, KSUB_8

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         LUNIT_8=LUNIT
         CALL UFBCNT_8(LUNIT_8,KMSG_8,KSUB_8)
         KMSG=KMSG_8
         KSUB=KSUB_8

         IM8B=.TRUE.
         RETURN
      ENDIF

C  CHECK THE FILE STATUS - RETURN THE MESSAGE AND SUBSET COUNTERS
C  --------------------------------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      KMSG = NMSG(LUN)
      KSUB = NSUB(LUN)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: UFBCNT - BUFR FILE IS CLOSED, IT MUST BE '//
     . 'OPEN FOR EITHER INPUT OR OUTPUT')
      END

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine ufbcnt().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine ufbcnt() directly.
C>
C> @author J. Ator
C> @date 2022-10-04
C>
C> @param[in] LUNIT_8  -- integer*8: Fortran logical unit number of
C>                        BUFR file
C> @param[out] KMSG_8 -- integer*8: Ordinal number of current message,
C>                       counting from the beginning of the BUFR file, but
C>                       not counting any messages which contain DX BUFR
C>                       tables information
C> @param[out] KSUB_8 -- integer*8: Ordinal number of current data subset
C>                       within (KMSG)th message, counting from the
C>                       beginning of the message
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-10-04 | J. Ator | Original author      |

      SUBROUTINE UFBCNT_8(LUNIT_8,KMSG_8,KSUB_8)

      INTEGER*8 LUNIT_8, KMSG_8, KSUB_8

      LUNIT=LUNIT_8
      CALL UFBCNT(LUNIT,KMSG,KSUB)
      KMSG_8=KMSG
      KSUB_8=KSUB

      RETURN
      END
