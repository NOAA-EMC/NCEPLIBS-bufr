C> @file
C> @brief Test whether a real*8 data value is "missing"

C> This function provides a handy way to check whether a real*8
C> data value returned from a previous call to any of the
C> subroutines ufbint(), ufbrep(), ufbseq(), ufbtab(), etc
C> contains the current placeholder value for "missing" data.
C>
C> @author J. Ator
C> @date 2007-01-19
C>
C> @param[in] R8VAL - real*8: Data value to be tested
C>
C> @returns ibfms  -  integer:
C>                    - 0 = R8VAL is not "missing"
C>                    - 1 = R8VAL is "missing"
C>
C> <p>The current placeholder value for "missing" data
C> is always equal to the value XMISS as specified during the
C> most recent call to subroutine setbmiss(), or to a default
C> value of 10E10 if setbmiss() was never called.  In either
C> case, a return value of 1 means that the corresponding
C> value was encoded as "missing" (all bits set to 1)
C> within the actual BUFR data subset.
C>
C> @remarks
C> - The use of an integer return code allows this function
C> to be called in a logical context from application programs
C> written in C as well as in Fortran.
C>
C> <b>Program history log:</b>
C> - 2007-01-19  J. Ator    -- Original author
C> - 2009-03-23  J. Ator    -- Increased value of BDIFD for better 
C>                           test accuracy
C> - 2012-10-05  J. Ator    -- Modified to reflect the fact that the
C>                           "missing" value is now configurable by
C>                           users (may be something other than 10E10)
C>
	INTEGER FUNCTION IBFMS ( R8VAL )

	INCLUDE	'bufrlib.inc'

	REAL*8		R8VAL

C----------------------------------------------------------------------
C----------------------------------------------------------------------

	IF ( R8VAL .EQ. BMISS ) THEN
	    IBFMS = 1
	ELSE
	    IBFMS = 0
	ENDIF

	RETURN
	END
