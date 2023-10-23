C> @file
C> @brief Encapsulate a BUFR message with IEEE Fortran control
C> words.
C> @author J. Woollen @date 2012-09-15

C> Encapsulate a BUFR message with IEEE Fortran
C> control words as specified via the most recent call to
C> subroutine setblock().
C>
C> A previous call to subroutine setblock() is required in
C> order to activate encapsulation with control words, and to
C> specify whether the control words should be encoded using
C> big-endian or little-endian byte ordering.  In such cases,
C> the input parameter MBAY is then modified to
C> add the specified control words to the existing BUFR message
C> whenever this subroutine is called, and MWRD is also
C> modified accordingly.
C>
C> Alternatively, if subroutine setblock() was never previously
C> called, or if no encapsulation was specified during the most
C> recent call to subroutine setblock(), then this subroutine
C> simply returns without modifying either of its input parameters.
C>
C> @param[in,out] MBAY -- integer(*): BUFR message, possibly with
C>                        added control words on output
C> @param[in,out] MWRD -- integer: Size (in integers) of contents
C>                        of MBAY
C>
C> @remarks
C> - For more information about IEEE Fortran control words, as
C> well as their historical use within the BUFRLIB software, see
C> the documentation for subroutine setblock().
C> - Whenever a BUFR message in MBAY is to be encapsulated with
C> control words, the user must ensure the availability of
C> sufficient extra space when allocating MBAY within the
C> application program.
C>
C> @author J. Woollen @date 2012-09-15
      SUBROUTINE BLOCKS(MBAY,MWRD)

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)
      COMMON /ENDORD/ IBLOCK,IORDBE(4),IORDLE(4)

      INTEGER*4 MBAY(MWRD),IINT,JINT

      CHARACTER*1 CINT(4),DINT(4)
      EQUIVALENCE(CINT,IINT)
      EQUIVALENCE(DINT,JINT)

      DATA IFIRST/0/
      SAVE IFIRST

c----------------------------------------------------------------------
c----------------------------------------------------------------------

      if(iblock.eq.0) return

      if(ifirst.eq.0) then

c        Initialize some arrays for later use.  Note that Fortran
c        record control words are always 4 bytes.

         iint=0; cint(1)=char(1)
         do i=1,4
         if(cint(1).eq.char(01)) then
            iordbe(i)=4-i+1
            iordle(i)=i
         else
            iordle(i)=4-i+1
            iordbe(i)=i
         endif
         enddo
         ifirst=1
      endif

c  make room in mbay for control words - one at each end of the record
c  -------------------------------------------------------------------

      if(nbytw.eq.8) mwrd=mwrd*2

      do m=mwrd,1,-1
      mbay(m+1) = mbay(m)
      enddo

c  store the endianized control word in bytes in dint/jint
c  -------------------------------------------------------

      iint=mwrd*4

      do i=1,4
      if(iblock.eq.+1) dint(i)=cint(iordbe(i))
      if(iblock.eq.-1) dint(i)=cint(iordle(i))
      enddo

c  increment mrwd and install the control words in their proper places
c  -------------------------------------------------------------------

      mwrd = mwrd+2
      mbay(1) = jint
      mbay(mwrd) = jint

      if(nbytw.eq.8) mwrd=mwrd/2

      return
      end
