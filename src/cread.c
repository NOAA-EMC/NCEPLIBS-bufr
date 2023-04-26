/** @file
 *  @brief C language interface for reading or writing BUFR messages.
 *
 *  This interface is used internally by many of the Fortran language
 *  [file-reading/writing subroutines](@ref hierarchy) and
 *  [message-reading/writing subroutines](@ref hierarchy) within
 *  the NCEPLIBS-bufr software, in order to use C to directly read/write
 *  BUFR messages from/to files.
 *
 *  This in turn eliminates the need for IEEE Fortran control words
 *  within files, though such control words can still be
 *  included when writing BUFR messages to files (if desired)
 *  via a previous call to subroutine setblock().
 *
 * @author J. Woollen @date 2012-09-15
 */

#include "bufrlib.h"
#include "cread.h"

/**
 * Open a new file for reading BUFR messages.
 *
 * @param nfile - File ID.
 * @param ufile - [path/]name of file to be opened.
 *
 * @author J. Woollen @date 2012-09-15
 */
void
openrb(int nfile, char *ufile) {
    pb[nfile] = fopen(ufile, "rb ");
}

/**
 * Open a new file for writing BUFR messages.
 *
 * @param nfile - File ID.
 * @param ufile - [path/]name of file to be opened.
 *
 * @author J. Woollen @date 2012-09-15
 */
void
openwb(int nfile, char *ufile) {
    pb[nfile] = fopen(ufile, "wb ");
}

/**
 * Open a new file for appending BUFR messages.
 *
 * @param nfile - File ID.
 * @param ufile - [path/]name of file to be opened.
 *
 * @author J. Woollen @date 2012-09-15
 */
void
openab(int nfile, char *ufile) {
    pb[nfile] = fopen(ufile, "a+b");
}

/**
 * Backspace a BUFR file by one BUFR message.
 *
 * @param nfile - File ID.
 *
 * @author J. Woollen @date 2012-09-15
 */
void
backbufr(int nfile) {
    fsetpos(pb[nfile],&lstpos[nfile]);
}

/**
 * Rewind a BUFR file back to its beginning.
 *
 * @param nfile - File ID.
 *
 * @author J. Woollen @date 2012-09-15
 */
void
cewind(int nfile) {
    rewind(pb[nfile]);
}

/**
 * Close a previously opened BUFR file.
 *
 * @param nfile - File ID.
 *
 * @author J. Woollen @date 2012-09-15
 */
void
closfb(int nfile) {
    fclose(pb[nfile]);
}

/**
 * Read the next message from a BUFR file that was previously opened
 * for reading.
 *
 * @param nfile - File ID.
 * @param bufr - BUFR message.
 * @param mxwrd - Number of elements in bufr array; used by the
 * function to ensure that it doesn't overflow the array.
 *
 * @returns 
 * - 0 normal return.
 * - -1 end-of-file encountered while reading.
 * - -2 I/O error encountered while reading.
 * - -3 overflow of bufr array.
 *
 * @author J. Woollen @date 2012-09-15
 */
int
crdbufr(int nfile, int *bufr, int mxwrd) {

    int nbytrem, nintrem, nbytx, nintx, wkint[2], ii;
    size_t nb = sizeof(int), nbdi8 = 8/sizeof(int);
    char wkchr[17] = "                ";
    fpos_t nxtpos;

    /* Find the start of the next BUFR message within the file. */
    fgetpos(pb[nfile], &lstpos[nfile]);
    while (strncmp(wkchr, "BUFR", 4) != 0) {
        memmove(wkchr, &wkchr[1], 3);
        if (fread(wkchr + 3, 1, 1, pb[nfile]) != 1)
            return -1;
    }

    /* Save the current location in case we need to restore it later. */
    fgetpos(pb[nfile], &nxtpos);

    /* Read the next 4 bytes of the message. */
    if (fread(wkchr+4, 1, 4, pb[nfile]) != 4)
        return -1;

    /* Determine the remaining number of bytes in the message. This doesn't
     * include the first 8 bytes, because we've already read those in. */
    memcpy(wkint, wkchr, 8);
    nbytrem = iupbs01_f(wkint, "LENM") - 8;

    nintrem = nbytrem / nb;
    nintx = nbdi8;
    /* Since nbytrem doesn't include the first 8 bytes of the BUFR message, then
     * nintrem is at least nintx integers short of the number needed to hold the
     * entire message.  But there may still be one more integer needed beyond
     * that, depending on whether nbytrem is an exact multiple of nb. */
    if ( ( nbytx = nbytrem % nb ) > 0 ) {
        nintx += 1;
    }
    /* Make sure the output array is large enough to hold the entire message. */
    if (nintrem + nintx > mxwrd) {
        fsetpos(pb[nfile], &nxtpos);
        return -3;
    }

    /* Copy the first 8 bytes of the BUFR message into the output array. */
    for ( ii = 0; ii < nbdi8; ii++ ) {
        bufr[ii] = wkint[ii];
    }
    /* Continue reading up to the next-to-last integer of the BUFR message,
     * and copy these into the output array. */
    if (fread(&bufr[ii], nb, nintrem-1, pb[nfile]) != nintrem-1) {
        fsetpos(pb[nfile],&nxtpos);
        return -2;
    }

    /* Read the last few bytes of the BUFR message and check for the "7777"
     * indicator. We want to read the end of the message byte-by-byte
     * (rather than integer-by-integer) so that we can easily check for the
     * ending "7777" string. */
    nbytrem = nb + nbytx;
    if (fread(wkchr, 1, nbytrem, pb[nfile]) != nbytrem) {
        fsetpos(pb[nfile], &nxtpos);
        return -2;
    }
    if (strncmp(&wkchr[nbytrem-4], "7777", 4) != 0) {
        fsetpos(pb[nfile], &nxtpos);
        return -2;
    }
    /* Copy the last few bytes of the BUFR message into the output array. */
    memcpy(wkint, wkchr, nbytrem);
    bufr[nintrem + 1] = wkint[0];
    if ( nbytx > 0 ) bufr[nintrem + 2] = wkint[1];

    return 0;
}

/**
 * Write a BUFR message into a file that was previously opened for
 * writing.
 *
 * @param nfile - File ID.
 * @param bufr  - BUFR message.
 * @param nwrd  - Size (in integers) of bufr.
 *
 * @author J. Woollen @date 2012-09-15
 */
void
cwrbufr(int nfile, int *bufr, int nwrd) {
    size_t nb = sizeof(int);

    fwrite(bufr, nb, nwrd, pb[nfile]);
}
