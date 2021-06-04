/** @file
 *  @brief Define signatures and declare variables for reading or writing BUFR
 *  messages via a C language interface.
 *  
 *  These signatures and variables are used by the C language interface which
 *  encompasses subroutines openrb(), openwb(), openab(), backbufr(), cewind(),
 *  closfb(), crdbufr() and cwrbufr().
 *
 *  The variables are dimensioned as one larger than NFILES because
 *  of the difference in array indexing between Fortran and C.
 *  In each subroutine, the value passed in for nfile will be an
 *  internal Fortran I/O stream index ranging in value from 1 to NFILES,
 *  so we need to allow for this same range of values in C, which would
 *  otherwise expect the array indices to range from 0 to NFILES-1.
 *
 *  @author J. Woollen
 *  @date 2012-09-15
 */
#ifdef UNDERSCORE
#define openrb     openrb_
#define openwb     openwb_
#define openab     openab_
#define backbufr   backbufr_
#define cewind     cewind_
#define closfb     closfb_
#define crdbufr    crdbufr_
#define cwrbufr    cwrbufr_
#endif

void openrb( f77int *, char * );
void openwb( f77int *, char * );
void openab( f77int *, char * );
void backbufr( f77int * );
void cewind( f77int * );
void closfb( f77int * );
f77int crdbufr( f77int *, char *, f77int * );
void cwrbufr( f77int *, f77int *, f77int * );

/** @var pb
 *  File pointers
 *
 *  @var lstpos
 *  Byte positions of last successful reads from files corresponding to pb,
 *  for files that were opened for reading
 */
#ifdef DYNAMIC_ALLOCATION

#   ifdef IN_ARALLOCC
	FILE **pb;
	fpos_t *lstpos;
#   else
	extern FILE **pb;
	extern fpos_t *lstpos;
#   endif

#else

    FILE *pb[NFILES+1];
    fpos_t lstpos[NFILES+1];

#endif
