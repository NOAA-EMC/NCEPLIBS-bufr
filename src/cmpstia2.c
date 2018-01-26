/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:   CMPSTIA2 
C   PRGMMR: ATOR             ORG: NCEP       DATE: 2017-11-13
C
C ABSTRACT:  THIS ROUTINE DEFINES A COMPARISON BETWEEN TWO ENTRIES IN
C   THE INTERNAL MEMORY STRUCTURE USED FOR STORING ASCII MASTER
C   CODE/FLAG TABLE INFORMATION.  THE COMPARISON IS USED BY THE
C   BINARY SEARCH FUNCTION BSEARCH, AND IT DIFFERS FROM THE LOGIC IN
C   ROUTINE CMPSTIA1 BECAUSE IT ONLY COMPARES THE IFFXYN AND IFVAL
C   COMPONENTS OF THE STRUCTURE, WHEREAS CMPSTIA1 COMPARES ALL OF THE
C   COMPONENTS IFFXY, IFVAL, IFFXYD and IFVALD.
C
C PROGRAM HISTORY LOG:
C 2017-11-13  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL CMPSTIA2( PE1, PE2 )
C   INPUT ARGUMENT LIST:
C     PE1      - FIRST STRUCTURE ENTRY TO BE COMPARED
C     PE2      - SECOND STRUCTURE ENTRY TO BE COMPARED
C
C   OUTPUT ARGUMENT LIST:
C     CMPSTIA2   - INTEGER: RESULT OF COMPARISON:
C                      -1 = PE1 is less than PE2
C                       0 = PE1 is equal to PE2
C                       1 = PE1 is greater than PE2
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: SRCHTBF
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

int cmpstia2( const void *pe1, const void *pe2 )
{
	struct code_flag_entry *mype1 = ( struct code_flag_entry * ) pe1;
	struct code_flag_entry *mype2 = ( struct code_flag_entry * ) pe2;

	if ( mype1->iffxyn == mype2->iffxyn ) {
	    if ( mype1->ifval == mype2->ifval ) return 0;
	    return ( mype1->ifval < mype2->ifval ? -1 : 1 );
	}
	else {
	    return ( mype1->iffxyn < mype2->iffxyn ? -1 : 1 );
	}
}
