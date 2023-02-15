/** @file
 *  @brief C language interface for reading or writing BUFR messages.
 *
 *  This interface is used internally by many of the Fortran language
 *  [file-reading/writing subroutines](@ref hierarchy) and
 *  [message-reading/writing subroutines](@ref hierarchy) within
 *  the BUFRLIB software, in order to use C to directly read/write
 *  BUFR messages from/to system files.
 *
 *  This in turn eliminates the need for IEEE Fortran control words
 *  within system files, though such control words can still be
 *  included when writing BUFR messages to system files (if desired)
 *  via a previous call to subroutine setblock().
 *
 * @author J. Woollen @date 2012-09-15
 */

#include "bufrlib.h"
#include "cread.h"

/**
 * This subroutine opens a new system file for reading BUFR messages.
 *
 * @param[in] nfile -- f77int*: Internal Fortran I/O stream index associated with ufile
 * @param[in] ufile -- char*: [path/]name of system file to be opened
 *
 * @author J. Woollen @date 2012-09-15
 */
void openrb   (nfile,ufile) f77int *nfile; char *ufile; { pb[*nfile] = fopen( ufile , "rb " ); }

/**
 * This subroutine opens a new system file for writing BUFR messages.
 *
 * @param[in] nfile -- f77int*: Internal Fortran I/O stream index associated with ufile
 * @param[in] ufile -- char*: [path/]name of system file to be opened
 *
 * @author J. Woollen @date 2012-09-15
 */
void openwb   (nfile,ufile) f77int *nfile; char *ufile; { pb[*nfile] = fopen( ufile , "wb " ); }

/**
 * This subroutine opens a new system file for appending BUFR messages.
 *
 * @param[in] nfile -- f77int*: Internal Fortran I/O stream index associated with ufile
 * @param[in] ufile -- char*: [path/]name of system file to be opened
 *
 * @author J. Woollen @date 2012-09-15
 */
void openab   (nfile,ufile) f77int *nfile; char *ufile; { pb[*nfile] = fopen( ufile , "a+b" ); }

/**
 * This subroutine backspaces a BUFR file by one BUFR message.
 *
 * @param[in] nfile -- f77int*: Internal Fortran I/O stream index associated with BUFR file
 *
 * @author J. Woollen @date 2012-09-15
 */
void backbufr (nfile      ) f77int *nfile;              { fsetpos(pb[*nfile],&lstpos[*nfile]);}

/**
 * This subroutine rewinds a BUFR file back to its beginning.
 *
 * @param[in] nfile -- f77int*: Internal Fortran I/O stream index associated with BUFR file
 *
 * @author J. Woollen @date 2012-09-15
 */
void cewind   (nfile      ) f77int *nfile;              { rewind(pb[*nfile]); }

/**
 * This subroutine closes a previously opened BUFR file.
 *
 * @param[in] nfile -- f77int*: Internal Fortran I/O stream index associated with BUFR file
 *
 * @author J. Woollen @date 2012-09-15
 */
void closfb   (nfile      ) f77int *nfile;              { fclose(pb[*nfile]); }

/**
 * This function reads the next message from a BUFR file that was previously opened for reading.
 *
 * @param[in] nfile -- f77int*: Internal Fortran I/O stream index associated with BUFR file
 * @param[out] bufr -- char*: BUFR message
 * @param[in] mxbyt -- f77int*: Dimensioned size (in bytes) of bufr; used by the function to
 *                     ensure that it doesn't overflow the BUFR array
 * @returns crdbufr -- f77int: return code
 *                     - 0 = normal return
 *                     - -1 = end-of-file encountered while reading
 *                     - -2 = I/O error encountered while reading
 *                     - -3 = overflow of bufr array
 *
 * @author J. Woollen @date 2012-09-15
 */
f77int crdbufr (nfile,bufr,mxbyt)
f77int *nfile; f77int *mxbyt; char *bufr;
{  f77int  nbyt; f77int  nb; f77int wkint[2]; fpos_t nxtpos;
   fgetpos(pb[*nfile],&lstpos[*nfile]);
   nb = sizeof(*bufr); bufr[0]=bufr[1];
   while ( strncmp(bufr,"BUFR",4)!=0)
   {  memmove(bufr,&bufr[1],3);
      if(fread(bufr+3,nb,1,pb[*nfile])!=1) return -1;
   }
   fgetpos(pb[*nfile],&nxtpos); if(fread(bufr+4,nb,4,pb[*nfile])!=4) return -1;
   memcpy(wkint,bufr,8); nbyt=iupbs01(wkint,"LENM",4)-8;
   if(nbyt+8>*mxbyt)                           {fsetpos(pb[*nfile],&nxtpos);return -3;};
   if(fread(bufr+8,nb,nbyt,pb[*nfile])!=nbyt)  {fsetpos(pb[*nfile],&nxtpos);return -2;};
   if(strncmp(bufr+nbyt+4,"7777",4)!=0)        {fsetpos(pb[*nfile],&nxtpos);return -2;};
   return 0;
}

/**
 * This subroutine writes a BUFR message into a file that was previously opened for writing.
 *
 * @param[in] nfile -- f77int*: Internal Fortran I/O stream index associated with BUFR file
 * @param[in] bufr  -- f77int*: BUFR message
 * @param[in] nwrd  -- f77int*: Size (in f77ints) of bufr
 *
 * @author J. Woollen @date 2012-09-15
 */
void cwrbufr (nfile,bufr,nwrd)
f77int *nfile; f77int *nwrd; f77int  *bufr;
{  f77int  nb; nb = sizeof(*bufr);
   fwrite(bufr,nb,*nwrd,pb[*nfile]);
}
