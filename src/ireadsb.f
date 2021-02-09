C> @file
C> @brief Read the next data subset from a BUFR message
      
C> This function calls BUFRLIB subroutine readsb() and 
C> passes back its return code as the function value.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT  - integer: Fortran logical unit number for
C>                         BUFR file
C> @returns ireadsb - integer:
C>                    - 0 = new BUFR data subset was successfully
C>                          read into internal arrays
C>                    - -1 = there are no more BUFR data subsets in
C>                           the BUFR message
C>
C> @remarks
C> - The use of this function allows the return code from readsb() to be
C> used as the target variable within an iterative program loop.
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 2002-05-14 J. Woollen -- Changed from an entry point to increase
C>                           portability to other platforms
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                           documentation
C>
C> <b>This routine calls:</b>  readsb()
C>
C> <b>This routine is called by:</b> ufbtab()
C>                     <br>Also called by application programs.
C>
      FUNCTION IREADSB(LUNIT)

      CALL READSB(LUNIT,IRET)
      IREADSB = IRET
      RETURN
      END
