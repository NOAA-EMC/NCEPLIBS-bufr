C> @file
C> @brief Read the next message from a BUFR file that was previously
C> opened for reading.

C> This function calls BUFRLIB subroutine readmg() and passes
C> back its return code as the function value.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT  - integer: Fortran logical unit number for BUFR file
C> @param[out] SUBSET   - character*8: Table A mnemonic for type of BUFR
C>                        message that was read
C>                       (see [DX BUFR Tables](@ref dfbftab)
C>                        for further information about Table A mnemonics)
C> @param[out] IDATE    - integer: date-time stored within Section 1 of
C>                        BUFR message that was read, in format of either
C>                        YYMMDDHH or YYYYMMDDHH, depending on the most
C>                        recent call to subroutine datelen()
C> @returns ireadmg - integer:
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
C> - 1994-01-06  J. Woollen -- Original author
C> - 1999-11-18  J. Woollen -- Added new function entry points ireadmm and
C>                             ireadibm
C> - 2002-05-14  J. Woollen -- Removed entry points icopysb, ireadft,
C>                             ireadibm, ireadmm, ireadns and ireadsb
C>                             (they became separate routines in the
C>                             BUFRLIB to increase portability to other
C>                             platforms)
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                             documentation
C>
C> <b>This routine calls:</b>        readmg()
C>
C> <b>This routine is called by:</b> ufbtab()
C>                            <br>Also called by application programs.
C>
      FUNCTION IREADMG(LUNIT,SUBSET,IDATE)

      CHARACTER*8 SUBSET
      CALL READMG(LUNIT,SUBSET,IDATE,IRET)
      IREADMG = IRET
      RETURN
      END
