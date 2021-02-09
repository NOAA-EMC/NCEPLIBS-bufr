C> @file
C> @brief Specify location of master BUFR tables on local file system

C> This subroutine allows the specification of the directory location
C> and Fortran logical unit numbers to be used when reading master
C> BUFR tables on the local file system.
C>
C> @author J. Ator
C> @date 2009-03-23
C>
C> @param[in] CMTDIR   - character*(*): Directory location of master
C>                          BUFR tables on local file system
C>                          (up to 100 characters)
C> @param[in] LUNMT1   - integer: First Fortran logical unit number
C>                          to use when reading master BUFR tables on
C>                          local file system
C> @param[in] LUNMT2   - integer: Second Fortran logical unit number
C>                          to use when reading master BUFR tables on
C>                          local file system
C>
C> <p>See [Master BUFR Tables](@ref dfbfmstab)
C> for more information about master BUFR tables.  In particular, note
C> that this subroutine is normally only ever called after a prior call
C> has been made to subroutine openbf() with IO = 'SEC3'.  But in such
C> cases, any call to this subroutine must be made prior to any
C> subsequent calls to any of the BUFR message-reading subroutines
C> (e.g. readmg(), readns(), readerme() ) for the associated BUFR file;
C> otherwise, default values for CMTDIR, LUNMT1 and LUNMT2 will be used
C> as defined within subroutine bfrini().  
C>
C> <p>For CMTDIR, any full or relative directory pathname that is legal
C> on the local filesystem is permissible, up to a total maximum length
C> of 100 characters.  The BUFRLIB software will then automatically search
C> within this directory for any necessary master table files and open and
C> read them as needed.
C> 
C> <p>The logical unit numbers LUNMT1 and LUNMT2 should be distinct from
C> each other but should not already be assigned to any files on the
C> local system.
C>
C> <b>Program history log:</b>
C> - 2009-03-23  J. Ator    -- Original author
C>
C> <b>This routine calls</b>: bort2()   strsuc()
C>
C> <b>This routine is called by:</b> None
C>                 <br>Normally called only by application programs.
C>
      SUBROUTINE MTINFO ( CMTDIR, LUNMT1, LUNMT2 )

      COMMON /MSTINF/ LUN1, LUN2, LMTD, MTDIR

      CHARACTER*(*) CMTDIR

      CHARACTER*128 BORT_STR
      CHARACTER*100 MTDIR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      CALL STRSUC ( CMTDIR, MTDIR, LMTD )
      IF ( LMTD .LT. 0 ) GOTO 900

      LUN1 = LUNMT1
      LUN2 = LUNMT2

C  EXITS
C  -----

      RETURN
900   BORT_STR = 'BUFRLIB: MTINFO - BAD INPUT MASTER TABLE DIRECTORY:'
      CALL BORT2(BORT_STR,CMTDIR)
      END
