/*
**  The arrays in this header are dimensioned one larger than NFILES because
**  of the difference in array indexing between Fortran and C.  In each of the
**  CREAD functions, the value passed in for nfile will be a Fortran index
**  ranging from 1 to NFILES, so we need to allow for this same range of
**  values in C, which would otherwise expect the array indices to range
**  from 0 to NFILES-1.
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
