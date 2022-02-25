C> @file
C> @brief Reset the BUFRLIB software for potential dynamic
C> reallocation of memory.

C> This subroutine frees all dynamically-allocated memory,
C> closes all logical units that are open within the
C> BUFRLIB software, and resets the library to all of its
C> default settings as though it had never been called.
C>   
C> @author J. Ator
C> @date 2015-03-02
C>
C> @remarks
C> - Calling this subroutine allows an application program to
C>   potentially resize arrays and reallocate memory all over again
C>   with a new subsequent series of calls to subroutines isetprm()
C>   and openbf().  However, if and when this subroutine is called,
C>   there is no longer any internal memory available within the
C>   BUFRLIB software, and the remainder of the library becomes
C>   essentially unusable within the application program, unless
C>   and until subroutine openbf() is called once again to
C>   dynamically allocate new array space.  This may be a useful
C>   capability for application programs that are finished with
C>   using the BUFRLIB software and wish to move on to other
C>   unrelated tasks without continuing to tie up all of the
C>   allocated memory space within the library.  Otherwise, and
C>   unless there's a need to change parameter sizes following the
C>   first call to subroutine openbf(), then there's no need to ever
C>   call this subroutine within an application program, since all
C>   allocated memory will automatically get freed anyway by the
C>   operating system once the application program terminates.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2015-03-02 | J. Ator | Original author |
C>
	SUBROUTINE EXITBUFR

	USE MODV_IFOPBF
	USE MODA_STBFR
	USE MODA_S01CM
        use subroutine_closbf

	COMMON /TABLEF/ CDMF

	CHARACTER*1 CDMF

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Close any logical units that are open to the library.

	DO JJ = 1, NFILES
	  IF ( IOLUN(JJ) .NE. 0 ) CALL CLOSBF( ABS(IOLUN(JJ)) )
	END DO

C	Deallocate all allocated memory.

	CALL ARDLLOCF

	IF ( CDMF .EQ. 'Y' ) CALL DLLOCTBF

C	Reset the library.

	NS01V = 0
	IFOPBF = 0

	RETURN
	END
