#include "cnvrrs.h"
#include "bstree.h"
#include "mrglvls.h"

void mrglvls( void ) {

    struct bstnode *root = NULL;

    f77r8 bmiss;

    f77int jj, ii, ntidx;

#define NLEV 4

    f77int topidxs[(1<<NLEV)-1];  /* the expression (1<<NLEV)-1 can be resolved
				     at compile time and results in the same
				     value as pow(float(2),float(NLEV))-1, which
				     can't be resolved at compile time */

    bmiss = getbmiss();

/*
**  Build the top NLEV levels of the binary search tree.  The higher NLEV is
**  set, the more balanced and efficient the resulting overall tree will be.
*/
    ntidx = treetop( 0, R66_BASE(nr66), NLEV, topidxs );
    for ( jj = 0; jj < ntidx; jj++ ) {
      root = addnode( root, 
	  R66_BASE(r66)[topidxs[jj]][0], R66_BASE(r66)[topidxs[jj]][1],
	  R66_BASE(r66)[topidxs[jj]][2], R66_BASE(r66)[topidxs[jj]][3], bmiss,
	  bmiss, R66_BASE(r66)[topidxs[jj]][4], R66_BASE(r66)[topidxs[jj]][5],
	  R66_BASE(r66)[topidxs[jj]][6], R66_BASE(r66)[topidxs[jj]][7] );
    }

/*    -- then call addnode for each of the nr66 rows of r66,  */

    for ( jj = 0; jj < R66_BASE(nr66); jj++ ) {
      root = addnode( root, 
	R66_BASE(r66)[jj][0], R66_BASE(r66)[jj][1], R66_BASE(r66)[jj][2],
	R66_BASE(r66)[jj][3], bmiss, bmiss, R66_BASE(r66)[jj][4], 
	R66_BASE(r66)[jj][5], R66_BASE(r66)[jj][6], R66_BASE(r66)[jj][7] );
    }

/*    -- then call addnode for each of the nr64 rows of r64,  */

    for ( jj = 0; jj < R64_BASE(nr64); jj++ ) {
      root = addnode( root, 
	R64_BASE(r64)[jj][0], 0., R64_BASE(r64)[jj][1],
	R64_BASE(r64)[jj][2], bmiss, bmiss, R64_BASE(r64)[jj][3],
	R64_BASE(r64)[jj][4], bmiss, bmiss );
    }

/*    -- then call addnode for each of the nr65 rows of r65  */

    for ( jj = 0; jj < R65_BASE(nr65); jj++ ) {
      root = addnode( root, 
	R65_BASE(r65)[jj][0], 0., bmiss, bmiss, R65_BASE(r65)[jj][1],
	R65_BASE(r65)[jj][2], bmiss, bmiss, R65_BASE(r65)[jj][3],
	R65_BASE(r65)[jj][4] );
    }

/*    -- then call readtree to read out the final sorted sounding,
	passing in root as argument */
    readtree( root, MXMRG, &MRG_BASE(nmrg), MRG_BASE(mrg) );

}
