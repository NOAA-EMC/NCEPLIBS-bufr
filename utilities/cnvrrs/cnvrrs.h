#include <stdio.h>
#include <stdlib.h>
#include <libgen.h>
#include <unistd.h>
#include <time.h>

/*
** In order to ensure that the C <-> FORTRAN interface works properly (and
** portably!), the default size of an "INTEGER" declared in FORTRAN must be 
** identical to that of an "int" declared in C, and a "double" declared in C
** must be 8 bytes just like a "REAL*8" declared in FORTRAN.
*/ 
#ifdef F77_INTSIZE_8
    typedef long f77int;
#else
    typedef int f77int;
#endif
typedef double f77r8;

/*
** On certain operating systems, the FORTRAN compiler appends an underscore
** to subprogram names in its object namespace.  Therefore, on such systems,
** a matching underscore must be appended to any C language references to
** the same subprogram names so that the linker can correctly resolve such
** references across the C <-> FORTRAN interface at link time.  This needs
** to be done for any subprogram that is either:
**   1) a C function, or
**   2) a FORTRAN subprogram called from C
*/
#ifdef UNDERSCORE
#define bvers bvers_
#define ccbfl ccbfl_
#define cobfl cobfl_
#define getbmiss getbmiss_
#define ibfms ibfms_
#define ctimdif ctimdif_
#define fcnvrrs fcnvrrs_
#define mrglvls mrglvls_
#define prtusage prtusage_
#endif

/*
** Function prototypes.
*/
void bvers( char *, f77int );
void ccbfl( void );
void cobfl( char *, char * );
f77r8 getbmiss( void );
f77int ibfms( f77r8 * );
void ctimdif( f77r8 *, f77r8 *, f77r8 *, f77r8 *, f77r8 *, f77r8 *,
	      f77r8 *, f77r8 *, f77r8 *, f77r8 *, f77r8 *, f77r8 *,
	      f77r8 * );
void fcnvrrs( char *, char *, char *, f77int, f77int, f77int );
void mrglvls( void );
void prtusage( char * );
