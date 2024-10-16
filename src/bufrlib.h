/** @file
 *  @brief Enable a number of NCEPLIBS-bufr subprograms to be called from within
 *  the C part of the library.
 *
 *  This header file defines signatures which wrap a number of native Fortran subprograms
 *  in the library.  It also contains prototypes for native C functions in the library as
 *  well as macros used throughout the C part of the library.
 *
 *  @author J. Ator @date 2003-11-04
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "bufr_interface.h"

void arallocc(void);
void ardllocc(void);
void dlloctbf(void);
void openrb(int nfile, char *ufile);
void openwb(int nfile, char *ufile);
void openab(int nfile, char *ufile);
void backbufr(int nfile);
void cewind(int nfile);
void closfb(int nfile);
int crdbufr(int nfile, int *bufr, int mxwrd);
void cwrbufr(int nfile, int *bufr, int nwrd);
int icvidx(int ii, int jj, int numjj);
void restd(int lunb, int tddesc, int *nctddesc, int *ctddesc);
void stseq(int lun, int *irepct, int idn, char *nemo, char *cseq, int *cdesc, int ncdesc);

/** Size of a character string needed to store an FXY value. */
#define FXY_STR_LEN 6

/** Character string containing minimum FXY value for a replication descriptor. */
#define MIN_FXY_REPL "101000"

/** Character string containing minimum FXY value for a Table D descriptor. */
#define MIN_FXY_TABLED "300000"

/** Character string containing maximum FXY value for a Table B descriptor. */
#define MAX_FXY_TABLEB "063255"

/** Size of a character string needed to store a mnemonic. */
#define NEMO_STR_LEN 8

/** Size of a character string needed to store the units of a Table B descriptor. */
#define UNIT_STR_LEN 24

/**
 * Convert an FXY value from its WMO bit-wise representation to its
 * six-character representation.
 *
 * Wraps cadn30() subroutine.
 *
 * @param idn - WMO bit-wise representation of FXY value.
 * @param adn - FXY value.
 * @param adn_str_len - Length of adn string.
 *
 * @author J. Ator @date 2004-08-18
 */
void cadn30_f(int idn, char *adn, int adn_str_len);

/**
 * Get the next index for storing an entry within an internal DX BUFR table.
 *
 * Wraps igetntbi() function.
 *
 * @param lun - File ID.
 * @param table_type - Type of internal DX BUFR table ('A', 'B', or 'D').
 *
 * @return Next available index within table_type.
 *
 * @author J. Ator @date 2009-03-23
 */
int igetntbi_f(int lun, char *table_type);

/**
 * Decode the scale factor, reference value, bit width, and units from a Table B
 * mnemonic definition.
 *
 * Wraps elemdx() subroutine.
 *
 * @param card - mnemonic definition card.
 * @param lun - File ID.
 *
 * @author J. Ator @date 2003-11-04
 */
void elemdx_f(char *card, int lun);

/**
 * Search for a Table B or Table D descriptor within the internal DX BUFR tables.
 *
 * Wraps numtbd() subroutine.
 *
 * @param lun - File ID.
 * @param idn - WMO bit-wise representation of FXY value.
 * @param nemo - Mnemonic.
 * @param nemo_str_len - Length of nemo string.
 * @param tab - Type of internal DX BUFR table ('B', or 'D').
 * @param iret - Positional index of idn within Table B or D, or 0 if not found.
 *
 * @author J. Ator @date 2003-11-04
 */
void numtbd_f(int lun, int idn, char *nemo, int nemo_str_len, char *tab, int *iret);

/**
 * Convert an FXY value from its 6 character representation to its WMO bit-wise
 * representation.
 *
 * Wraps ifxy() function.
 *
 * @param cfxy - FXY value.
 *
 * @return WMO bit-wise representation of FXY value.
 *
 * @author J. Ator @date 2023-04-07
 */
int ifxy_f(char *cfxy);

/**
 * Get the WMO bit-wise representation of the FXY value corresponding
 * to a child mnemonic of a Table D sequence.
 *
 * Wraps uptdd() subroutine.
 *
 * @param id - Positional index of parent mnemonic within internal Table D.
 * @param lun - File ID.
 * @param ient - Ordinal indicator of child mnemonic to be returned, or 0 to
 * request a count of the total number of child mnemonics.
 * @param iret - Total number of child mnemonics if ient = 0; otherwise
 * the WMO bit-wise representation of the FXY value corresponding to
 * the ient'th mnemonic.
 *
 * @author J. Ator @date 2003-11-04
 */
void uptdd_f(int id, int lun, int ient, int *iret);

/**
 * Check whether a specified mnemonic is a Table C marker operator.
 *
 * Wraps imrkopr() function.
 *
 * @param nemo - Mnemonic.
 *
 * @return
 * - 0 nemo is not a Table C marker operator.
 * - 1 nemo is a Table C marker operator.
 *
 * @author J. Ator @date 2016-05-04
 */
int imrkopr_f(char *nemo);

/**
 * Check whether a descriptor is WMO-standard.
 *
 * Wraps istdesc() function.
 *
 * @param idn - WMO bit-wise representation of FXY value for descriptor.
 *
 * @return
 * - 0 idn is not a WMO-standard descriptor.
 * - 1 idn is a WMO-standard descriptor.
 *
 * @author J. Ator @date 2004-08-18
 */
int istdesc_f(int idn);

/**
 * Decode an integer from a character string.
 *
 * Wraps strnum() subroutine.
 *
 * @param str - String.
 * @param num - Value decoded from str.
 * @param iret - Return code: 0 if successful, -1 otherwise.
 *
 * @author J. Ator @date 2003-11-04
 */
void strnum_f(char *str, int *num, int *iret);

/**
 * Store a new entry within the internal BUFR Table B or D.
 *
 * Wraps stntbi() subroutine.
 *
 * @param n - Storage index into internal Table B or D.
 * @param lun - File ID.
 * @param numb - FXY number for new entry.
 * @param nemo - mnemonic corresponding to numb.
 * @param celsq - Element or sequence definition corresponding to numb.
 *
 * @author J. Ator @date 2009-03-23
 */
void stntbi_f(int n, int lun, char *numb, char *nemo, char *celsq);

/**
 * Get the next usable Table D index for the current master table, or
 * reset the index.
 *
 * Wraps igettdi() function.
 *
 * @param iflag - Processing flag; if 0 will reset the index.
 *
 * @return
 * - -1, if iflag was input as 0.
 * - next usable scratch Table D index, otherwise.
 *
 * @author J. Ator @date 2009-03-23
 */
int igettdi_f(int iflag);

/**
 * Store information about a child mnemonic within the internal arrays.
 *
 * Wraps pktdd() subroutine.
 *
 * @param id - Index of parent mnemonic within internal arrays.
 * @param lun - File ID.
 * @param idn - WMO bit-wise representation of FXY value for child mnemonic,
 * or 0 to delete all child mnemonic information for parent mnemonic id.
 * @param iret - 0 if idn=0; -1 if error occurred; otherwise, the total number of
 * child mnemonics stored so far for parent mnemonic id.
 *
 * @author J. Ator @date 2003-11-04
 */
void pktdd_f(int id, int lun, int idn, int *iret);

/**
 * Log one error message and abort application program.
 *
 * Wraps bort() subroutine.
 *
 * @param errstr - Error message.
 *
 * @author J. Ator @date 2023-04-07
 */
void bort_f(char *errstr);
