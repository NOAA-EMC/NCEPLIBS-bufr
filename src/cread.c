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

    int nbytrem, nintrem, wkint[2];
    size_t nb = sizeof(int);
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

    /* Determine the remaining number of bytes in the message. This
     * doesn't include the first 8 bytes (i.e. the first 2 4-byte
     * integers) of the BUFR message, because we've already read those
     * in. */
    memcpy(wkint, wkchr, 8);
    nbytrem = iupbs01_f(wkint, "LENM") - 8;

    /* Continue reading up to the last few bytes of the message. The
     * value of nintrem will always be at least 2 and no more than 3
     * 4-byte integers short of the number needed to contain the
     * entire BUFR message. */
    nintrem = nbytrem / nb;
    if (nintrem + 3 > mxwrd) {
        fsetpos(pb[nfile], &nxtpos);
        return -3;
    }
    bufr[0] = wkint[0];
    bufr[1] = wkint[1];
    if (fread(&bufr[2], nb, nintrem-1, pb[nfile]) != nintrem-1) {
        fsetpos(pb[nfile],&nxtpos);
        return -2;
    }

    /* Read the last few bytes of the message and check for the "7777"
     * indicator. We want to read the last few bytes of the messages
     * byte-by-byte (rather than integer-by-integer) so that we can
     * easily check for the ending "7777" string. */
    nbytrem = nb + nbytrem % nb;
    if (fread(wkchr, 1, nbytrem, pb[nfile]) != nbytrem) {
        fsetpos(pb[nfile], &nxtpos);
        return -2;
    }
    if (strncmp(&wkchr[nbytrem-4], "7777", 4) != 0) {
        fsetpos(pb[nfile], &nxtpos);
        return -2;
    }
    memcpy(wkint, wkchr, 8);
    bufr[nintrem + 1] = wkint[0];
    bufr[nintrem + 2] = wkint[1];

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
    int nb;

    nb = sizeof(*bufr);
    fwrite(bufr, nb, nwrd, pb[nfile]);
}
