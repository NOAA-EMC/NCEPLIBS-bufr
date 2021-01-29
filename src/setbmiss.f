C> @file
C> @brief Define a customized placeholder value for "missing" data

C> This subroutine allows the user to specify a customized value to
C> represent "missing" data when reading from or writing to BUFR files.
C>
C> @author J. Woollen
C> @date 2012-09-15
C>
C> @param[in] XMISS - real*8: new placeholder value to represent
C>                    "missing" data
C>
C> <p>This subroutine can be called at any time from within an
C> application program, and the value XMISS will then be treated as
C> "missing" when reading or writing BUFR data during all future
C> calls to subroutines ufbint(), ufbrep(), ufbseq(), etc.
C> Otherwise, if this subroutine is never called, a default
C> placeholder value of 10E10 is used for "missing", as set via
C> an initial internal call to subroutine bfrini().
C>
C> <p>Any data value can always be checked for equivalence to the
C> current "missing" value via a call to function ibfms().  See also
C> function getbmiss().
C>
C> @remarks
C> - The value XMISS is never actually encoded within a BUFR data
C> subset; rather, XMISS is a user-friendly placeholder value to
C> represent "missing" data values within the scope of the
C> application program.  In any actual BUFR data subset, "missing"
C> values are always encoded as all bits set to 1, per WMO
C> regulations.
C> 
C> <b>Program history log:</b>
C> - 2012-09-15  J. Woollen -- Original author
C>
C> <b>This routine calls:</b> openbf()
C>
C> <b>This routine is called by:</b>None
C>                     <br>Normally called only by application programs.
C>
      SUBROUTINE SETBMISS(XMISS)

      INCLUDE 'bufrlib.inc'

      REAL*8 XMISS

c-----------------------------------------------------------------------
c-----------------------------------------------------------------------

      CALL OPENBF(0,'FIRST',0)

      BMISS = XMISS

      RETURN
      END
