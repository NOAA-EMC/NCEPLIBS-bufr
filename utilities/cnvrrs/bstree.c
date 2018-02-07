#include "cnvrrs.h"
#include "bstree.h"

/*
**  This module contains functions used in building, traversing and reading data
**  from a binary search tree.  Each node of the tree will contain the data for
**  a different level of the merged sounding, using ltds as the key value.
**  A binary search tree provides the optimal balance between efficient
**  searchability for data within the tree (as in an ordered array) vs. the
**  ability to easily add new data nodes when needed (as in a linked list).
*/

/*  based on the given index range ibeg->iend, determine the indices for the top nlev levels
**  of the binary search tree.  By evenly dividing the range across more and more top levels
**  initially, we end up with a more and more evenly balanced binary search tree which makes
**  later searches and inserts more and more efficient. Note that idx must be dimensioned at
**  least as large as ((2**(nlev))-1).  Return value is number of indices returned.
*/
f77int treetop( f77int ibeg, f77int iend, f77int nlev, f77int *idx )
{

/*
**  The following variable is declared as static so that it
**  automatically initializes to zero and remains in scope
**  through successive recursive calls to this function.
*/
    static f77int nidx;

    f77int imid;

    if ( nlev > 0 ) {
	imid = ibeg + ( iend - ibeg ) / 2;
	idx[nidx++] = imid;
	nidx = treetop( ibeg, imid, nlev-1, idx );
	nidx = treetop( imid, iend, nlev-1, idx );
    }

    return nidx;
}


/* Add a node to the binary search tree, using ltds as the key value.  If such a node
already exists, then fill in any "missing" data values at that node if possible. */

struct bstnode *addnode( struct bstnode *root,
	f77r8 ltds, f77r8 vsigx, f77r8 prlc, f77r8 gph10, f77r8 latdh,
	f77r8 londh, f77r8 tmdb, f77r8 tmdp, f77r8 wdir, f77r8 wspd )
{
    if ( root == NULL ) {
	root = ( struct bstnode * ) malloc( sizeof( struct bstnode ) );
        if ( root == NULL ) {
	    printf( "ERROR - Could not allocate memory for binary search tree node!\n" );
	    exit(EXIT_FAILURE);
	}
	else {
	    root->left = NULL;
	    root->right = NULL;
	    root->ltds = ltds;
	    root->vsigx = vsigx;
	    root->prlc = prlc;
	    root->gph10 = gph10;
	    root->latdh = latdh;
	    root->londh = londh;
	    root->tmdb = tmdb;
	    root->tmdp = tmdp;
	    root->wdir = wdir;
	    root->wspd = wspd;
	}
    }
    else if ( ltds < root->ltds ) {
	root->left = addnode( root->left,
		ltds, vsigx, prlc, gph10, latdh,
		londh, tmdb, tmdp, wdir, wspd );
    }
    else if ( ltds > root->ltds ) {
	root->right = addnode( root->right,
		ltds, vsigx, prlc, gph10, latdh,
		londh, tmdb, tmdp, wdir, wspd );
    }
    else {
        /* if possible, fill in any "missing" values at this existing node */
	if ( vsigx > 0 ) root->vsigx = vsigx;
	if ( ibfms( &root->prlc ) ) root->prlc = prlc;
	if ( ibfms( &root->gph10 ) ) root->gph10 = gph10;
	if ( ibfms( &root->latdh ) ) root->latdh = latdh;
	if ( ibfms( &root->londh ) ) root->londh = londh;
	if ( ibfms( &root->tmdb ) ) root->tmdb = tmdb;
	if ( ibfms( &root->tmdp ) ) root->tmdp = tmdp;
	if ( ! ibfms( &wdir ) ) root->wdir = wdir;
	if ( ! ibfms( &wspd ) ) root->wspd = wspd;
    }
    return root;
}

/* this will read the binary search tree (bst) in sorted order by using an inorder traversal
scheme, but can only do this one time since free() is called inside of here to free the
memory space for each node once it has been read*/

void readtree( struct bstnode *root, f77int mxnode, f77int *nnode, f77r8 (*nodedata)[10] ) {

    if ( root != NULL ) {

	readtree( root->left, mxnode, nnode, nodedata );

	if ( *nnode < mxnode ) {
	  nodedata[*nnode][0] = root->ltds;
	  nodedata[*nnode][1] = root->vsigx;
	  nodedata[*nnode][2] = root->prlc;
	  nodedata[*nnode][3] = root->gph10;
	  nodedata[*nnode][4] = root->latdh;
	  nodedata[*nnode][5] = root->londh;
	  nodedata[*nnode][6] = root->tmdb;
	  nodedata[*nnode][7] = root->tmdp;
	  nodedata[*nnode][8] = root->wdir;
	  nodedata[*nnode][9] = root->wspd;
	  (*nnode)++;
	}

	readtree( root->right, mxnode, nnode, nodedata );

	free( root );
    }
}
