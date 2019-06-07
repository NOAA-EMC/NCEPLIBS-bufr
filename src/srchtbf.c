/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SRCHTBF
C   PRGMMR: ATOR             ORG: NCEP       DATE: 2018-01-11
C
C ABSTRACT:  THIS ROUTINE SEARCHES FOR A SPECIFIED DESCRIPTOR AND
C   ASSOCIATED VALUE (CODE FIGURE OR BIT NUMBER) WITHIN THE INTERNAL
C   MEMORY STRUCTURE FOR STORING CODE/FLAG TABLE INFORMATION.  THE
C   SEARCH MAY ALSO OPTIONALLY INCLUDE A SPECIFIED SECOND DESCRIPTOR
C   AND ASSOCIATED VALUE UPON WHICH THE FIRST DESCRIPTOR AND ITS
C   ASSOCIATED VALUE DEPEND, FOR CASES SUCH AS, E.G. WHEN THE MEANING
C   OF AN ORIGINATING SUBCENTER VALUE DEPENDS ON THE IDENTITY OF THE
C   ORIGINATING CENTER.
C
C   IF THE REQUESTED ENTRY IN THE TABLE IS FOUND, THE ROUTINE RETURNS
C   THE ASSOCIATED MEANING AS A CHARACTER STRING.  OTHERWISE, AND IF
C   THERE WAS NO OPTIONAL SECOND DESCRIPTOR AND ASSOCIATED VALUE
C   SPECIFIED ON INPUT, THE ROUTINE WILL RE-SEARCH THE TABLE TO CHECK
C   WHETHER THE MEANING OF THE FIRST DESCRIPTOR AND ASSOCIATED VALUE
C   MAY INDEED DEPEND ON THE VALUE OF ONE OR MORE OTHER POSSIBLE
C   SECOND DESCRIPTORS.  IF SO, THOSE POSSIBLE DESCRIPTORS ARE RETURNED
C   ALONG WITH A SPECIAL RETURN CODE SO THAT THE CALLING ROUTINE MAY
C   EXAMINE THEM AND POSSIBLY ISSUE ANOTHER SUBSEQUENT CALL TO THIS
C   SAME ROUTINE WITH SPECIFIED VALUES FOR THE SECOND DESCRIPTOR AND
C   ASSOCIATED VALUE.
C
C PROGRAM HISTORY LOG:
C 2018-01-11  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL SRCHTBF ( IFXYI, IVALI, IFXYD, MXFXYD, IVALD,
C                          MEANING, MXMNG, LNMNG, IRET )
C
C   INPUT ARGUMENT LIST:
C     IFXYI    - INTEGER: BIT-WISE REPRESENTATION OF FXY DESCRIPTOR
C     IVALI    - INTEGER: VALUE (CODE FIGURE OR BIT NUMBER) ASSOCIATED
C                WITH IFXYI
C     IFXYD    - INTEGER: ARRAY WITH THE FIRST ELEMENT SET TO THE
C                BIT-WISE REPRESENTATION OF THE FXY DESCRIPTOR, IF ANY,
C                UPON WHICH THE VALUES IFXYI AND IVALI DEPEND.  THIS IS
C                OPTIONAL, AND THE FIRST ELEMENT OF THE ARRAY CAN BE SET
C                TO (-1) IF THE MEANINGS OF IFXYI AND IVALI DO NOT
C                DEPEND ON THE VALUE OF ANY OTHER DESCRIPTOR.
C                  -1 = NO DEPENDENCY SPECIFIED
C     IVALD    - INTEGER: VALUE (CODE FIGURE OR BIT NUMBER) ASSOCIATED
C                WITH IFXYD.  THIS VALUE SHOULD BE SET TO (-1)
C                WHENEVER THE FIRST ELEMENT OF THE IFXYD ARRAY IS
C                LIKEWISE SET TO (-1)
C                  -1 = NO DEPENDENCY SPECIFIED
C     MXFXYD   - INTEGER: DIMENSIONED SIZE OF IFXYD; USED BY THE ROUTINE
C                TO ENSURE THAT IT DOES NOT OVERFLOW THE IFXYD ARRAY
C                UPON OUTPUT
C     MXMNG    - INTEGER: DIMENSIONED SIZE OF MEANING STRING; USED BY
C                THE ROUTINE TO ENSURE THAT IT DOES NOT OVERFLOW THIS
C                STRING UPON OUTPUT
C
C   OUTPUT ARGUMENT LIST:
C     MEANING  - CHARACTER*(LNMNG): MEANING CORRESPONDING TO IFXYI AND
C                IVALI (AND TO IFXYD AND IVALD, IF SPECIFIED)
C     LNMNG    - INTEGER: LENGTH OF STRING RETURNED IN MEANING
C     IFXYD    - INTEGER: IF THE INITIAL SEARCH OF THE TABLE WAS
C                UNSUCCESSFUL, *AND* IF NO OPTIONAL SECOND DESCRIPTOR
C                AND ASSOCIATED VALUE WERE SPECIFIED ON INPUT, *AND* IF
C                THE SECOND SEARCH OF THE TABLE DETERMINED THAT THE
C                MEANING OF THE FIRST DESCRIPTOR AND ASSOCIATED VALUE
C                INDEED DEPENDS ON ONE OR MORE OTHER POSSIBLE SECOND
C                DESCRIPTORS, THEN THOSE POSSIBLE SECOND DESCRIPTORS
C                ARE RETURNED WITHIN THE FIRST IRET ELEMENTS OF IFXYD 
C     IRET     - RETURN CODE:
C                   0 = MEANING FOUND AND STORED IN MEANING STRING
C                  -1 = MEANING NOT FOUND
C                  >0 = MEANING NOT FOUND, *AND* IFXYD AND IVALD WERE
C                       BOTH SET TO (-1) ON INPUT, *AND* THE MEANING OF
C                       IFXYI AND IVALI DEPENDS ON THE VALUE OF ONE OF
C                       THE IRET DESCRIPTORS RETURNED IN IFXYD
C
C REMARKS:
C    THIS ROUTINE CALLS:        CMPSTIA1 CMPSTIA2
C    THIS ROUTINE IS CALLED BY: GETCFMNG UFDUMP
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#include "bufrlib.h"
#include "cfe.h"

void srchtbf( f77int *ifxyi, f77int *ivali, f77int *ifxyd, f77int *mxfxyd, f77int *ivald,
	      char *meaning, f77int *mxmng, f77int *lnmng, f77int *iret )
{
	struct code_flag_entry key, *pkey, *pcfe, *pbs;

	int ipt, ii, slmng;

	*iret = -1;

	/*
	**  Initialize some values for searching the internal table.
	*/

	key.iffxyn = *ifxyi;
	key.ifval = *ivali;
	key.iffxynd = ifxyd[0];
	key.ifvald = *ivald;

	pkey = &key;
	pcfe = &cfe[0];

	/*
	**  Search for a matching entry.
	*/
        pbs = ( struct code_flag_entry * ) bsearch( pkey, pcfe, ( size_t ) nmtf,
			sizeof( struct code_flag_entry ),
			( int (*) ( const void *, const void * ) ) cmpstia1 );
        if ( pbs != NULL ) {
	    /*
	    **  A matching entry was found, so set the appropriate output
	    **  values and return.
	    */
	    ipt = pbs - pcfe;
	    slmng = strlen( cfe[ipt].ifmeaning );
	    *lnmng = ( *mxmng > slmng ? slmng : *mxmng );
	    strncpy( meaning, &cfe[ipt].ifmeaning[0], *lnmng );
	    *iret = 0;
	    return;
	}

	/*
	**  Was a particular dependency specified in the input?
	*/
	if ( key.iffxynd != -1 ) {
	    /*
	    **  YES, so there's nothing else to do.
	    */
	    return;
	}
	
	/*
	**  NO, so check whether the given Table B descriptor and value have any
	**  dependencies, and if so then return a list of those dependencies.
	*/
        pbs = ( struct code_flag_entry * ) bsearch( pkey, pcfe, ( size_t ) nmtf,
			sizeof( struct code_flag_entry ),
			( int (*) ( const void *, const void * ) ) cmpstia2 );
        if ( pbs == NULL ) {
	    /*
	    **  There are no dependencies.
	    */
	    return;
	}

	/*
	**  Store the dependency that was returned by the secondary search.
	**  However, there may be others within the internal table, so we'll
	**  also need to check for those.
	*/
	ipt = pbs - pcfe;
	*iret = 0;
	ifxyd[(*iret)++] = cfe[ipt].iffxynd;

	/*
	**  Since the internal table is sorted, check immediately before and
	**  after the returned dependency for any additional table entries which
	**  correspond to the same Table B descriptor and value, but for which the
	**  dependency is different.  If any such additional dependencies are
	**  found, return those as well.
	*/
	ii = ipt - 1;
	while ( ( ii >= 0 ) &&
		( *iret < *mxfxyd ) &&
		( cfe[ii].iffxyn == key.iffxyn ) &&
		( cfe[ii].ifval == key.ifval ) ) {
	    if ( cfe[ii].iffxynd < ifxyd[(*iret)-1] )
		    ifxyd[(*iret)++] = cfe[ii].iffxynd;
	    ii--;
	}
	ii = ipt + 1;
	while ( ( ii < nmtf ) &&
		( *iret < *mxfxyd ) &&
		( cfe[ii].iffxyn == key.iffxyn ) &&
		( cfe[ii].ifval == key.ifval ) ) {
	    if ( ( cfe[ii].iffxynd > ifxyd[(*iret)-1] ) &&
		 ( cfe[ii].iffxynd > cfe[ipt].iffxynd ) )
		    ifxyd[(*iret)++] = cfe[ii].iffxynd;
	    ii++;
	}

	return;
}
