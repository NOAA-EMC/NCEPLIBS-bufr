C> @file
C> @brief Convert an FXY value from its WMO bit-wise
C> representation to its six-character representation.
C>
C> @author J. Ator @date 2004-08-18

C> Convert an FXY value from its WMO bit-wise representation
C> to its six-character representation.
C>
C> This subroutine converts an FXY value from its WMO bit-wise
C> representation to its 6 character representation.  It is similar
C> to function adn30(), except that it always returns 6 characters,
C> and it always returns its output as a call parameter instead of a
C> function value, which in turn allows it to be more easily called
C> from within a C language function.
C>
C> For a description of the WMO bit-wise representation of an FXY
C> value, see ifxy().
C>
C> @param[in] IDN - integer: Bit-wise representation of FXY value.
C> @param[out] ADN - character*6: FXY value.
C>
C> @author J. Ator @date 2004-08-18
        SUBROUTINE CADN30( IDN, ADN )

        CHARACTER*(*)   ADN
        CHARACTER*6     ADN30

        ADN = ADN30( IDN, 6 )

        RETURN
        END
