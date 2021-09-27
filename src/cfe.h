/** @file
 *  @brief Define signatures and declare variables for internal storage
 *  of master Code/Flag table entries.
 *
 *  @author J. Ator
 *  @date 2017-11-16
 */
#define MAX_MEANING_LEN 150

#ifdef UNDERSCORE
#define cmpstia1   cmpstia1_
#define cmpstia2   cmpstia2_
#define dlloctbf   dlloctbf_
#define inittbf    inittbf_
#define sorttbf    sorttbf_
#define srchtbf    srchtbf_
#define strtbfe    strtbfe_
#endif

int cmpstia1( const void *, const void * );
int cmpstia2( const void *, const void * );
void dlloctbf( void );
void inittbf( void );
void sorttbf( void );
void srchtbf( f77int *, f77int *, f77int *, f77int *, f77int *,
        char *, f77int *, f77int *, f77int * );
void strtbfe( f77int *, f77int *, char *, f77int *, f77int *, f77int * );

/**
 *  This structure contains array and variable declarations
 *  used to store a master Code/Flag table entry.
 */
struct code_flag_entry {
    /** @var iffxyn
     *  Bit-wise representation of FXY number to which this entry belongs.
     *
     *  @var ifval
     *  Code figure or bit number.
     *
     *  @var ifmeaning
     *  Meaning corresponding to ifval.
     *
     *  @var iffxynd
     *  Bit-wise representation of FXY number upon which this entry is
     *  dependent, if any.  Set to (-1) if no dependency.
     *
     *  @var ifvald
     *  Code figure or bit number upon which this entry is dependent,
     *  if any.  Set to (-1) if no dependency.
     */
    f77int iffxyn;
    f77int ifval;
    char ifmeaning[MAX_MEANING_LEN+1];
    f77int iffxynd;
    f77int ifvald;
};

/** @var cfe
 *  Master Code/Flag table entries.
 *
 *  @var mxmtbf
 *  Maximum number of master Code/Flag table entries, counting across all
 *  individual Code/Flag tables, and counting each defined code figure
 *  (within each individual Code table) or defined bit number (within
 *  each individual Flag table) as a separate entry.
 *
 *  @var nmtf
 *  Number of stored master Code/Flag table entries in cfe, up to a
 *  maximum of MXMTBF.
 */
#ifdef IN_INITTBF
    struct code_flag_entry *cfe;  /* will automatically initialize to NULL */
    f77int mxmtbf;
    f77int nmtf;
#else
    extern struct code_flag_entry *cfe;
    extern f77int mxmtbf;
    extern f77int nmtf;
#endif
