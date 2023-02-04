/** @file
 *  @brief Store a new master Code/Flag table entry.
 */
#include "bufrlib.h"
#include "cfe.h"

/**
 *  This subroutine adds a new entry to the internal memory
 *  structure for storage of master Code/Flag table entries.
 *
 *  @author J. Ator
 *  @date 2017-11-13
 *
 *  @param[in] ifxyn -- f77int*: Bit-wise representation of FXY number
 *                      for which ival is a defined code or flag
 *                      table entry
 *  @param[in] ival  -- f77int*: Code figure or bit number
 *  @param[in] meaning -- char*: Meaning associated with ifxyn and ival
 *  @param[in] lmeaning -- f77int*: Length (in bytes) of meaning
 *  @param[in] idfxy -- f77int*: Bit-wise representation of FXY number
 *                      upon which ifxyn and ival depend (if any), or
 *                      else set to a value of (-1)
 *  @param[in] idval -- f77int*: Code figure or bit number associated
 *                      with idfxy and upon which ifxyn and ival depend
 *                      (if any), or else set to (-1) whenever idfxy is
 *                      also set to (-1)
 *
 * <b>Program history log:</b>
 * | Date | Programmer | Comments |
 * | -----|------------|----------|
 * | 2017-11-13 | J. Ator | Original author |
*/
void strtbfe( f77int *ifxyn, f77int *ival, char *meaning, f77int *lmeaning,
              f77int *idfxy, f77int *idval )
{
    unsigned int mnlen;

    static char brtstr[50] = "BUFRLIB: STRTBFE - MXMTBF OVERFLOW";

    /*
    **  Confirm that there's room for another entry in the structure.
    */
    if ( nmtf >= mxmtbf ) bort( brtstr, ( f77int ) strlen( brtstr ) );

    /*
    **  Store the new entry.
    */
    cfe[nmtf].iffxyn = *ifxyn;
    cfe[nmtf].ifval = *ival;
    mnlen = ( *lmeaning > MAX_MEANING_LEN ? MAX_MEANING_LEN : *lmeaning );
    strncpy( &cfe[nmtf].ifmeaning[0], meaning, mnlen );
    cfe[nmtf].ifmeaning[mnlen] = '\0';
    cfe[nmtf].iffxynd = *idfxy;
    cfe[nmtf].ifvald  = *idval;
    nmtf++;
}
