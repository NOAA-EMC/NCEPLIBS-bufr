#ifdef UNDERSCORE
#define treetop treetop_
#define addnode addnode_
#define readtree readtree_
#endif

/*
** Define the contents of a node on the binary search tree.
*/

struct bstnode {
    struct bstnode *left;
    struct bstnode *right;
    f77r8 ltds;
    f77r8 vsigx;
    f77r8 prlc;
    f77r8 gph10;
    f77r8 latdh;
    f77r8 londh;
    f77r8 tmdb;
    f77r8 tmdp;
    f77r8 wdir;
    f77r8 wspd;
};

/*
** Function prototypes.
*/
f77int treetop( f77int, f77int, f77int, f77int * );
struct bstnode *addnode( struct bstnode *,
	f77r8, f77r8, f77r8, f77r8, f77r8,
	f77r8, f77r8, f77r8, f77r8, f77r8 );
void readtree( struct bstnode *, f77int, f77int *, f77r8 (*)[10] );
