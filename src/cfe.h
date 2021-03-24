/** @file
 *  @brief Declare variables for internal storage of master Code/Flag
 *  table entries.
 */

/**
 *  This header file contains the structure and variable declarations
 *  used to store master Code/Flag table entries internally.
 *
 *  @author J. Ator
 *  @date 2017-11-16
 */

#define MAX_MEANING_LEN 150

struct code_flag_entry {
    f77int iffxyn;
    /** @var iffxyn
     *  Bitwise representation of FXY number to which this entry belongs.
     */
    f77int ifval;
    /** @var ifval
     *  Code figure or bit number.
     */
    char ifmeaning[MAX_MEANING_LEN+1];
    /** @var ifmeaning
     *  Meaning corresponding to ifval.
     */
    f77int iffxynd;
    /** @var iffxynd
     *  Bitwise representation of FXY number upon which this entry is
     *  dependent, if any.  Set to (-1) if no dependency.
     */
    f77int ifvald;
    /** @var ifvald
     *  Code figure or bit number upon which this entry is dependent,
     *  if any.  Set to (-1) if no dependency.
     */
};

/** @var cfe
 *  Master Code/Flag table entries.
 *
 *  @var mxmtbf
 *  Maximum number of master Code/Flag table entries that can be stored.
 *
 *  @var nmtf
 *  Number of stored master Code/Flag table entries in cfe.
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
