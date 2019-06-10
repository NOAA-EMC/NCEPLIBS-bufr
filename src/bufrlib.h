#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*
** The following value must be identically defined in Fortran source
** file modv_NFILES.F
*/
#ifdef DYNAMIC_ALLOCATION
#    define NFILES 32
#else
#    define NFILES 32
#endif

/*
** The following value must be identically defined in Fortran source
** file modv_MAXCD.F
*/
#ifdef DYNAMIC_ALLOCATION
#    define MAXCD 250
#else
#    define MAXCD 250
#endif

/*
** The following values must be identically defined in source file
** bufrlib.prm
*/
#define MAXNC 600
#define MXNAF 3

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
#define arallocc   arallocc_
#define ardllocc   ardllocc_
#define bort	   bort_
#define bort_exit  bort_exit_
#define cadn30	   cadn30_
#define ccbfl	   ccbfl_
#define cmpia	   cmpia_
#define cmpstia1   cmpstia1_
#define cmpstia2   cmpstia2_
#define cobfl	   cobfl_
#define cpmstabs   cpmstabs_
#define crbmg	   crbmg_
#define cwbmg	   cwbmg_
#define dlloctbf   dlloctbf_
#define elemdx	   elemdx_
#define gets1loc   gets1loc_
#define ichkstr	   ichkstr_
#define icvidx	   icvidx_
#define ifxy	   ifxy_
#define igetntbi   igetntbi_
#define igetprm    igetprm_
#define igettdi	   igettdi_
#define imrkopr	   imrkopr_
#define inittbf    inittbf_
#define ipkm	   ipkm_
#define istdesc	   istdesc_
#define iupbs01	   iupbs01_
#define iupm	   iupm_
#define nemtab	   nemtab_
#define nemtbb	   nemtbb_
#define nummtb	   nummtb_
#define numtbd	   numtbd_
#define pktdd	   pktdd_
#define rbytes	   rbytes_
#define restd	   restd_
#define sorttbf    sorttbf_
#define srchtbf    srchtbf_
#define stntbi	   stntbi_
#define strnum	   strnum_
#define strtbfe	   strtbfe_
#define stseq	   stseq_
#define uptdd	   uptdd_
#define wrdesc	   wrdesc_
#define wrdlen	   wrdlen_
#define openrb     openrb_
#define openwb     openwb_
#define openab     openab_
#define backbufr   backbufr_
#define cewind     cewind_
#define closfb     closfb_
#define crdbufr    crdbufr_
#define cwrbufr    cwrbufr_
#endif

/*
** In order to ensure that the C <-> FORTRAN interface works properly (and
** portably!), the default size of an "INTEGER" declared in FORTRAN must be 
** identical to that of an "int" declared in C.  If this is not the case (e.g.
** some FORTRAN compilers, most notably AIX via the -qintsize= option, allow the
** sizes of INTEGERs to be definitively prescribed outside of the source code
** itself!), then the following conditional directive (or a variant of it) can
** be used to ensure that the size of an "int" in C remains identical to that
** of an "INTEGER" in FORTRAN.
*/ 
#ifdef F77_INTSIZE_8
    typedef long f77int;
#else
    typedef int f77int;
#endif

/*
** Declare prototypes for ANSI C compatibility.  This should be done for any
** subprogram that is either:
**   1) a C function, or
**   2) a FORTRAN subprogram called from C
*/
void arallocc( void );
void ardllocc( void );
void bort( char *, f77int );
void bort_exit( void );
void cadn30( f77int *, char *, f77int ); 
void ccbfl( void );
int cmpia( const void *, const void * );
int cmpstia1( const void *, const void * );
int cmpstia2( const void *, const void * );
void cobfl( char *, char * );
void cpmstabs( f77int *, f77int *, char (*)[4], char (*)[12], char (*)[4],
	char (*)[14], char (*)[8], char (*)[120], f77int *, f77int *,
	char (*)[120], char (*)[8], f77int *, f77int *, f77int * );
void crbmg( char *, f77int *, f77int *, f77int * );
void cwbmg( char *, f77int *, f77int * );
void dlloctbf( void );
void elemdx( char *, f77int *, f77int );
void gets1loc( char *, f77int *, f77int *, f77int *, f77int *, f77int );
f77int ichkstr ( char *, char *, f77int *, f77int, f77int );
f77int icvidx ( f77int *, f77int *, f77int * );
f77int ifxy( char *, f77int );
f77int igetntbi( f77int *, char *, f77int );
f77int igetprm( char *, f77int );
f77int igettdi( f77int * );
f77int imrkopr( char *, f77int );
void inittbf( void );
void ipkm( char *, f77int *, f77int *, f77int );
f77int istdesc( f77int * );
f77int iupbs01 ( f77int *, char *, f77int );
f77int iupm ( char *, f77int *, f77int );
void nemtab( f77int *, char *, f77int *, char *, f77int *, f77int, f77int );
void nemtbb( f77int *, f77int *, char *, f77int *, f77int *, f77int *, f77int );
void nummtb( f77int *, char *, f77int * );
void numtbd( f77int *, f77int *, char *, char *, f77int *, f77int, f77int );
void pktdd( f77int *, f77int *, f77int *, f77int * );
f77int rbytes( char *, f77int *, f77int, f77int );
void restd( f77int *, f77int *, f77int *, f77int * );
void sorttbf( void );
void srchtbf( f77int *, f77int *, f77int *, f77int *, f77int *,
	char *, f77int *, f77int *, f77int * );
void stntbi( f77int *, f77int *, char *, char *, char *, f77int, f77int, f77int );
void strnum( char *, f77int *, f77int );
void strtbfe( f77int *, f77int *, char *, f77int *, f77int *, f77int * );
void stseq( f77int *, f77int *, f77int *, char *, char *, f77int *, f77int * );
void uptdd( f77int *, f77int *, f77int *, f77int * );
void wrdesc( f77int, f77int *, f77int * );
void wrdlen( void );
