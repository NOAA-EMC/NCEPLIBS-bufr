C> @file
C> @brief Test whether a real*8 data value is "missing"
C>
C> @author J. Ator @date 2007-01-19

C> Test whether a real*8 data value is "missing"
C>
C> This function provides a handy way to check whether a real*8
C> data value returned from a previous call to any of the
C> other BUFRLIB [values-reading subroutines](@ref hierarchy)
C> contains the current placeholder value for "missing" data.
C>
C> @param[in] R8VAL - real*8: Data value to be tested.
C> @returns ibfms - integer:
C> - 0 = R8VAL is not "missing".
C> - 1 = R8VAL is "missing".
C>
C> The current placeholder value for "missing" data
C> is always equal to the value XMISS as specified during the
C> most recent call to subroutine setbmiss(), or to a default
C> value of 10E10 if setbmiss() was never called.  In either
C> case, a return value of 1 means that the corresponding
C> value was encoded as "missing" (all bits set to 1)
C> within the actual BUFR data subset.
C>
C> @author J. Ator @date 2007-01-19
        INTEGER FUNCTION IBFMS ( R8VAL )

        USE MODV_BMISS

        REAL*8          R8VAL

C----------------------------------------------------------------------
C----------------------------------------------------------------------

        IF ( R8VAL .EQ. BMISS ) THEN
            IBFMS = 1
        ELSE
            IBFMS = 0
        ENDIF

        RETURN
        END
