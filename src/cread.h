/** @file
 *  @brief Declare variables for reading or writing BUFR
 *  messages via a C language interface.
 *
 *  These variables are used by the C language interface which
 *  encompasses functions openrb(), openwb(), openab(), backbufr(),
 *  cewind(), closfb(), crdbufr() and cwrbufr().
 *
 *  When allocated, these variables are dimensioned as one larger than
 *  NFILES because of the difference in array indexing between Fortran and C.
 *  In each function, the value passed in for nfile will be a Fortran file ID
 *  ranging in value from 1 to NFILES, so we need to allow for this same range
 *  of values in C, which would otherwise expect the array indices to range
 *  from 0 to NFILES-1.
 *
 *  @author J. Woollen
 *  @date 2012-09-15
 */

/** @var pb
 *  File pointers
 *
 *  @var lstpos
 *  Byte positions of last successful reads from files corresponding to pb,
 *  for files that were opened for reading
 */
#ifdef IN_ARALLOCC
    FILE **pb;
    fpos_t *lstpos;
#else
    extern FILE **pb;
    extern fpos_t *lstpos;
#endif
