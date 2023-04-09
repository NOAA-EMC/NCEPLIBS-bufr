/** @file
 * @brief Compute a unique 1-dimensional array index from
 * 2-dimensional indices.
 * @author J. Ator @date 2009-03-23
 */

/**
 * Computes a unique 1-dimensional array
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
 * @author J. Ator @date 2009-03-23
 */
int
icvidx(int ii, int jj, int numjj)
{
   return ( numjj * ii ) + jj;
}
