C> @file
C> @brief Return a copy of an input integer word with
C> the bytes reversed.
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 2003-11-04 | J. Ator    | Added documentation.
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | Unified/portable for wrf; added history documentation.
C> 2007-01-19 | J. Ator    | Big-endian vs. little-endian determined at compile time.
C>
C> @author Woollen @date 1994-01-06

C> This function will, when the local machine is "little-endian" (i.e.,
C> when it uses a right to left scheme for numbering the bytes
C> within a machine word), return a copy of an input integer word with
C> the bytes reversed. Although, by definition (within WMO Manual
C> 306), a BUFR message is a stream of individual octets (i.e., bytes)
C> that is independent of any particular machine representation, the
C> BUFR archive library software often needs to interpret all or parts
C> of two or more adjacent bytes in order to construct an integer
C> word. By default, the software uses the "big-endian" (left to
C> right) scheme for numbering bytes. By reversing the bytes, irev()
C> allows the integer word to be properly read or written (depending
C> on whether input or output operations, respectively, are being
C> performed) on "little-endian" machines. If the local machine is
C> "big-endian", irev() simply returns a copy of the same integer that was
C> input.
C>
C> @param[in] N - integer: integer word with bytes ordered according
C> to the "big-endian" numbering scheme
C>
C> @return - integer: integer word with bytes ordered according to
C> the numbering scheme of the local machine (either
C> "big-endian" or "little-endian"; if "big-endian" then
C> this is just a direct copy of N).
C>
C> @author Woollen @date 1994-01-06
      FUNCTION IREV(N)

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      CHARACTER*8 CINT,DINT
      EQUIVALENCE(CINT,INT)
      EQUIVALENCE(DINT,JNT)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

#ifdef BIG_ENDIAN
      IREV = N
#else
      INT = N
      DO I=1,NBYTW
        DINT(I:I) = CINT(IORD(I):IORD(I))
      ENDDO
      IREV = JNT
#endif

      RETURN
      END
