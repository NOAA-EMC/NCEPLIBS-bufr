C> @file
C> @brief Specify the use of IEEE Fortran control words when writing
C> BUFR messages.

C> This subroutine is used to specify whether BUFR messages output by
C> future calls to [message-writing subroutines](@ref hierarchy)
C> should be encapsulated with IEEE Fortran control words when being
C> written to output files.
C>
C> <p>If control words are requested, then one 4-byte control word is
C> written to the output file prior to the start of each BUFR message,
C> and a second 4-byte control word is written to the output file after
C> the end of each BUFR message.  Each of these control words contains
C> the byte count for the enclosed BUFR message, and they can be
C> written using either big-endian or little-endian byte ordering,
C> regardless of the native endianness of the local machine.
C>
C> <p>This subroutine can be called at any time after the first call
C> to subroutine openbf(), and the specified value for IBLK will remain
C> in effect for all future calls to
C> [message-writing subroutines](@ref hierarchy) for all Fortran logical
C> units that are open for output within the application program,
C> unless a subsequent call is made to this subroutine to reset the
C> value of IBLK again.  If this subroutine is never called, a default
C> value of 0 is used for IBLK, as set within subroutine bfrini().
C>
C> @author J. Woollen
C> @date 2012-09-15
C>
C> @param[in]  IBLK -- integer: Flag indicating whether future BUFR
C>                     output messages should be encapsulated with
C>                     control words
C>                      - -1 = Yes, using little-endian control words      
C>                      -  0 = No (the default)
C>                      -  1 = Yes, using big-endian control words      
C>
C> @remarks
C> - This subroutine can be used to generate BUFR files consistent
C> with historical archives, dating back to older versions of the
C> BUFRLIB software which used Fortran to directly read/write
C> BUFR messages from/to system files.  Standard Fortran historically
C> didn't have a way to read/write binary data streams without
C> control words, so as a result many historical archives contain
C> these by default.  However, newer versions of the BUFRLIB software
C> use C to directly read/write BUFR messages from/to system files
C> (including historical archives), so control words are no longer
C> necessary and are therefore now disabled by default when writing
C> BUFR messages to output files.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2012-09-15 | J. Woollen | Original author |
C> | 2022-10-04 | J. Ator | Added 8-byte wrapper |

      RECURSIVE SUBROUTINE SETBLOCK(IBLK) 

      USE MODV_IM8B

      COMMON /ENDORD/ IBLOCK,IORDBE(4),IORDLE(4)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(IBLK,MY_IBLK,1)
         CALL SETBLOCK(MY_IBLK)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL OPENBF(0,'FIRST',0)
      IBLOCK=IBLK  

      RETURN
      END
