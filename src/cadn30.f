C> @file
C> @brief Convert an FXY value from its bit-wise (integer)
C> representation to its six character representation

C> This subroutine converts an FXY value from its bit-wise (integer)
C> representation to its 6 character representation.  It is similar
C> to function adn30(), except that it always returns 6 characters,
C> and it always returns its output as a call parameter instead of a
C> function value, which in turn allows it to be more easily called
C> from within a C language function.
C>
C> @author J. Ator
C> @date 2004-08-18
C>
C> @param[in] IDN - integer: Bit-wise representation of FXY value
C> @param[out] ADN - character*6: FXY value
C>
C> <b>Program History Log:</b>
C> - 2004-08-18  J. Ator -- Original author
C>
	SUBROUTINE CADN30( IDN, ADN )

	CHARACTER*(*)	ADN
	CHARACTER*6	ADN30

	ADN = ADN30( IDN, 6 )

	RETURN
	END
