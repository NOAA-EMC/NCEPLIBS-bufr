C> @file
C> @brief Declare arrays for internal storage of long character strings.
C>
C> @author J. Ator @date 2014-02-05

C> This module contains array and variable declarations needed to store
C> long character strings (greater than 8 bytes) via subroutine hold4wlc().
C> These strings can be held for writing into an uncompressed BUFR data
C> subset via future internal calls to subroutine writlc().
C>
C> @author J. Ator @date 2014-02-05

        MODULE MODA_H4WLC

          USE MODV_MXH4WLC

C>    Number of long character strings being stored.
          INTEGER :: NH4WLC
C>    I/O stream index into internal arrays for associated output file.
          INTEGER :: LUH4WLC(MXH4WLC)
C>    Table B mnemonics associated with long character strings.
          CHARACTER*14 :: STH4WLC(MXH4WLC)
C>    Long character strings.
          CHARACTER*120 :: CHH4WLC(MXH4WLC)

        END MODULE
