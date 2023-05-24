C> @file
C> @brief Specify location of master BUFR tables on local file system.
C>
C> @author J. Ator @date 2009-03-23

C> Specify the directory location
C> and Fortran logical unit numbers to be used when reading master
C> BUFR tables on the local file system.
C>
C> @param[in] CMTDIR - character*(*): Directory location of master
C> BUFR tables on local file system (up to 240 characters).
C> @param[in] LUNMT1 - integer: First Fortran logical unit number
C> to use when reading master BUFR tables on local file system.
C> @param[in] LUNMT2 - integer: Second Fortran logical unit number
C> to use when reading master BUFR tables on local file system.
C>
C> See [Master BUFR Tables](@ref dfbfmstab)
C> for more information about master BUFR tables.  In particular, note
C> that this subroutine is normally only ever called after a prior call
C> has been made to subroutine openbf() with IO = 'SEC3'.  But in such
C> cases, any call to this subroutine must be made prior to any
C> subsequent calls to any of the BUFR
C> [message-reading subroutines](@ref hierarchy) for the associated BUFR
C> file; otherwise, default values for CMTDIR, LUNMT1 and LUNMT2 will be
C> used as defined within subroutine bfrini().
C>
C> For CMTDIR, any full or relative directory pathname that is legal
C> on the local filesystem is permissible, up to a total maximum length
C> of 240 characters.  The library will then automatically search
C> within this directory for any necessary master table files and open and
C> read them as needed.
C>
C> The logical unit numbers LUNMT1 and LUNMT2 should be distinct from
C> each other but should not already be assigned to any files on the
C> local system.
C>
C> @author J. Ator @date 2009-03-23
      RECURSIVE SUBROUTINE MTINFO ( CMTDIR, LUNMT1, LUNMT2 )

      USE MODV_IM8B

      COMMON /MSTINF/ LUN1, LUN2, LMTD, MTDIR

      CHARACTER*(*) CMTDIR

      CHARACTER*128 BORT_STR
      CHARACTER*240 MTDIR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------
      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84 ( LUNMT1, MY_LUNMT1, 1 )
         CALL X84 ( LUNMT2, MY_LUNMT2, 1 )
         CALL MTINFO ( CMTDIR, MY_LUNMT1, MY_LUNMT2 )

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL STRSUC ( CMTDIR, MTDIR, LMTD )

      LUN1 = LUNMT1
      LUN2 = LUNMT2

C  EXITS
C  -----

      RETURN
      END
