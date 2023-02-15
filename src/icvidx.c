/** @file
 * @brief Compute a unique 1-dimensional array index from
 * 2-dimensional indices.
 * @author Ator @date 2009-03-23
 */

#include "bufrlib.h"

/**
 * This routine computes a unique 1-dimensional array
 * index from 2-dimensional indices. This allows a 2-dimensional
 * (row-by-column) array to be stored and accessed as a
 * 1-dimensional array.
 *
 * @param ii - first (row) index
 * @param jj - second (column) index
 * @param numjj - maximum number of column indices
 *
 * @return 1-dimensional index.
 *
 * @author Ator @date 2009-03-23
 */
f77int icvidx( f77int *ii, f77int *jj, f77int *numjj )
{
        return ( *numjj * (*ii) ) + *jj;
}
