#define MAX_MEANING_LEN 150

struct code_flag_entry {
    f77int iffxyn;
	/* Bitwise representation of FXY number to which this entry belongs. */
    f77int ifval;
	/* Code figure or bit number. */
    char ifmeaning[MAX_MEANING_LEN+1];
	/* Meaning corresponding to ifval. */
    f77int iffxynd;
	/* Bitwise representation of FXY number upon which this entry is
	   dependent, if any.  Set to (-1) if no dependency. */
    f77int ifvald;
	/* Code figure or bit number upon which this entry is dependent,
	   if any.  Set to (-1) if no dependency. */
};

#ifdef IN_INITTBF
    struct code_flag_entry *cfe;  /* will automatically initialize to NULL */
    f77int mxmtbf;
    f77int nmtf;
#else
    extern struct code_flag_entry *cfe;
    extern f77int mxmtbf;
    extern f77int nmtf;
#endif
