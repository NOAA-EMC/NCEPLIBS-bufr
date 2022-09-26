C> @file
C> @brief Read the next message from a BUFR file that was previously
C> opened for reading.

C> This function calls BUFRLIB subroutine readmg() and passes
C> back its return code as the function value.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR file
C> @param[out] SUBSET  -- character*8: Table A mnemonic for type of BUFR
C>                        message that was read
C>                       (see [DX BUFR Tables](@ref dfbftab)
C>                        for further information about Table A mnemonics)
C> @param[out] IDATE   -- integer: date-time stored within Section 1 of
C>                        BUFR message that was read, in format of either
C>                        YYMMDDHH or YYYYMMDDHH, depending on the most
C>                        recent call to subroutine datelen()
C> @returns ireadmg -- integer: Return code:
C>                        - 0 = new BUFR message was successfully
C>                              read into internal arrays
C>                        - -1 = there are no more BUFR messages in the
C>                               file connected to logical unit LUNIT
C>
C> @remarks
C> - The use of this function allows the return code from readmg() to be
C> used as the target variable within an iterative program loop.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1999-11-18 | J. Woollen | Added new function entry points ireadmm and ireadibm |
C> | 2002-05-14 | J. Woollen | Removed entry points icopysb, ireadft, ireadibm, ireadmm, ireadns and ireadsb (they became separate routines in the BUFRLIB) |
C> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
C> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added history documentation |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      FUNCTION IREADMG(LUNIT,SUBSET,IDATE)

      USE MODV_IM8B

      CHARACTER*8 SUBSET

      INTEGER*8 LUNIT_8,IDATE_8,IREADMG_8

      IF(IM8B) THEN
         IM8B=.FALSE.

         LUNIT_8=LUNIT 
         IREADMG=IREADMG_8(LUNIT_8,SUBSET,IDATE_8)
         IDATE=IDATE_8

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL READMG(LUNIT,SUBSET,IDATE,IRET)
      IREADMG = IRET

      END FUNCTION

C> This function is an internal wrapper for handling 8-byte integer
C> arguments to subroutine ireadmg().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this function directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call function ireadmg() directly.
C>
C> @author J. Woollen
C> @date 2022-08-04
C>
C> @param[in] LUNIT_8 -- integer*8: Fortran logical unit number for
C>                       BUFR file
C> @param[out] SUBSET  -- character*8: Table A mnemonic for type of BUFR
C>                        message that was read
C>                       (see [DX BUFR Tables](@ref dfbftab)
C>                        for further information about Table A mnemonics)
C> @param[out] IDATE_8 -- integer*8: date-time stored within Section 1 of
C>                        BUFR message that was read, in format of either
C>                        YYMMDDHH or YYYYMMDDHH, depending on the most
C>                        recent call to subroutine datelen()
C> @returns ireadmg_8 -- integer*8: Return code
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-08-04 | J. Woollen | Original author      |

      FUNCTION IREADMG_8(LUNIT_8,SUBSET,IDATE_8)

      CHARACTER*8 SUBSET

      INTEGER*8 LUNIT_8,IDATE_8,IREADMG_8

      LUNIT=LUNIT_8 
      IREADMG_8=IREADMG(LUNIT,SUBSET,IDATE) 
      IDATE_8=IDATE

      END FUNCTION  
