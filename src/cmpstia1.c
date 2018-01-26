/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:   CMPSTIA1 
C   PRGMMR: ATOR             ORG: NCEP       DATE: 2017-11-13
C
C ABSTRACT:  THIS ROUTINE DEFINES A COMPARISON BETWEEN TWO ENTRIES IN
C   THE INTERNAL MEMORY STRUCTURE USED FOR STORING ASCII MASTER
C   CODE/FLAG TABLE INFORMATION.  THE COMPARISON IS USED BY THE
C   BINARY SEARCH FUNCTIONS QSORT AND BSEARCH, AND IT DIFFERS FROM THE
C   LOGIC IN ROUTINE CMPSTIA2 BECAUSE IT COMPARES ALL OF THE IFFXY,
C   IFVAL, IFFXYD and IFVALD COMPONENTS OF THE STRUCTURE, WHEREAS
C   CMPSTIA2 ONLY COMPARES THE IFFXYN AND IFVAL COMPONENTS.
C
C PROGRAM HISTORY LOG:
C 2017-11-13  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL CMPSTIA1( PE1, PE2 )
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
C    THIS ROUTINE IS CALLED BY: SORTTBF  SRCHTBF
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

int cmpstia1( const void *pe1, const void *pe2 )
{
	struct code_flag_entry *mype1 = ( struct code_flag_entry * ) pe1;
	struct code_flag_entry *mype2 = ( struct code_flag_entry * ) pe2;

	if ( mype1->iffxyn == mype2->iffxyn ) {
	    if ( mype1->ifval == mype2->ifval ) {
		if ( mype1->iffxynd == mype2->iffxynd ) {
		    if ( mype1->ifvald == mype2->ifvald ) return 0;
		    return ( mype1->ifvald < mype2->ifvald ? -1 : 1 );
		}
		else {
		    return ( mype1->iffxynd < mype2->iffxynd ? -1 : 1 );
		}
	    }
	    else {
		return ( mype1->ifval < mype2->ifval ? -1 : 1 );
	    }
	}
	else {
	    return ( mype1->iffxyn < mype2->iffxyn ? -1 : 1 );
	}
}
