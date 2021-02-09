C> @file
C> @brief Read the next data subset from a BUFR file that was
C> previously opened for reading.

C> This function calls BUFRLIB subroutine readns() and passes
C> back its return code as the function value.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT    - integer: Fortran logical unit number for
C>                       BUFR file
C> @param[out] SUBSET   - character*8: Table A mnemonic for type of
C>                        data subset that was read
C>                        (see [DX BUFR Tables](@ref dfbftab)
C>                        for further information about Table A mnemonics)
C> @param[out] IDATE    - integer: Date-time stored within Section 1 of
C>                        BUFR message containing data subset that
C>                        was read, in format of either
C>                        YYMMDDHH or YYYYMMDDHH, depending on the most
C>                        recent call to subroutine datelen()      
C> @returns ireadns - integer:
C>                    - 0 = new BUFR data subset was successfully
C>                          read into internal arrays
C>                    - -1 = there are no more BUFR data subsets in
C>                          the file connected to logical unit LUNIT
C>
C> @remarks
C> - The use of this function allows the return code from readns() to be
C> used as the target variable within an iterative program loop.
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 2002-05-14 J. Woollen -- Changed from an entry point to increase
C>                           portability to other platforms
C>
C> <b>This routine calls:</b> readns()
C>
C> <b>This routine is called by:</b> None
C>                 <br>Normally called only by application programs.
C>
      FUNCTION IREADNS(LUNIT,SUBSET,IDATE)

      CHARACTER*8 SUBSET
      CALL READNS(LUNIT,SUBSET,IDATE,IRET)
      IREADNS = IRET
      RETURN
      END
