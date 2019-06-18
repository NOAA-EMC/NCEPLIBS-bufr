/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:   STSEQ 
C   PRGMMR: ATOR             ORG: NP12       DATE: 2009-03-23
C
C ABSTRACT:  USING THE BUFR MASTER TABLES, THIS ROUTINE STORES ALL
C   OF THE INFORMATION FOR SEQUENCE IDN WITHIN THE INTERNAL BUFR
C   TABLES B AND D.  ANY DESCRIPTORS IN IDN WHICH ARE THEMSELVES
C   SEQUENCES ARE IMMEDIATELY RESOLVED VIA A RECURSIVE CALL TO THIS
C   SAME ROUTINE.
C
C PROGRAM HISTORY LOG:
C 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
C 2010-03-19  J. ATOR    -- ADDED PROCESSING FOR 2-04 ASSOCIATED FIELDS
C 2010-04-05  J. ATOR    -- ADDED PROCESSING FOR 2-2X, 2-3X AND 2-4X
C                           NON-MARKER OPERATORS
C 2015-03-04  J. ATOR    -- HANDLE SPECIAL CASE WHEN ASSOCIATED FIELDS
C                           ARE IN EFFECT FOR A TABLE D DESCRIPTOR
C
C USAGE:    CALL STSEQ( LUN, IREPCT, IDN, NEMO, CSEQ, CDESC, NCDESC )
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     IREPCT   - INTEGER: REPLICATION SEQUENCE COUNTER FOR THE CURRENT
C                MASTER TABLE; USED INTERNALLY TO KEEP TRACK OF WHICH
C                SEQUENCE NAMES HAVE ALREADY BEEN DEFINED AND THEREBY
C                AVOID CONTENTION WITHIN THE INTERNAL BUFR TABLE D
C     IDN      - INTEGER: BIT-WISE REPRESENTATION OF FXY VALUE FOR
C                SEQUENCE TO BE STORED
C     NEMO     - CHARACTER*8: MNEMONIC CORRESPONDING TO IDN
C     CSEQ     - CHARACTER*55: DESCRIPTION CORRESPONDING TO IDN
C     CDESC    - INTEGER: ARRAY OF BIT-WISE REPRESENTATIONS OF FXY
C                VALUES CORRESPONDING TO DESCRIPTORS WHICH CONSTITUTE
C                THE IDN SEQUENCE
C     NCDESC   - INTEGER: NUMBER OF VALUES IN CDESC
C
C   OUTPUT ARGUMENT LIST:
C     IREPCT   - INTEGER: REPLICATION SEQUENCE COUNTER FOR THE CURRENT
C                MASTER TABLE; USED INTERNALLY TO KEEP TRACK OF WHICH
C                SEQUENCE NAMES HAVE ALREADY BEEN DEFINED AND THEREBY
C                AVOID CONTENTION WITHIN THE INTERNAL BUFR TABLE D
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     CADN30   ELEMDX   ICVIDX
C                               IFXY     IGETNTBI IGETPRM  IGETTDI
C                               IMRKOPR  NEMTAB   NUMMTB   NUMTBD
C                               PKTDD    STNTBI   STRNUM   STSEQ
C    THIS ROUTINE IS CALLED BY: READS3   STSEQ
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#include "bufrlib.h"
#include "mstabs.h"

void stseq( f77int *lun, f77int *irepct, f77int *idn, char nemo[8],
	    char cseq[55], f77int cdesc[], f77int *ncdesc )
{
    f77int i, j, nb, nd, ipt, ix, iy, iret, nbits;
    f77int i0 = 0, imxcd, rpidn, pkint, ilen;

    char tab, adn[7], adn2[7], nemo2[9], units[10], errstr[129];
    char rpseq[56], card[80], cblk = ' ';

/*
**  The following variable is declared as automatic so that a local
**  private copy is created (and, if necessary, dynamically allocated)
**  during each recursive call to this subroutine.
*/
#ifdef DYNAMIC_ALLOCATION
    f77int *rpdesc;
#else
    f77int rpdesc[MAXCD];
#endif

/*
**  The following variables are declared as static so that they
**  automatically initialize to zero and remain unchanged between
**  recursive calls to this subroutine.
*/
    static f77int naf, iafpk[MXNAF];

/*
**  Is *idn already listed as an entry in the internal Table D?
**  If so, then there's no need to proceed any further.
*/
    numtbd( lun, idn, nemo2, &tab, &iret, sizeof( nemo2 ), sizeof( tab ) );
    if ( ( iret > 0 ) && ( tab == 'D' ) ) return;

/*
**  Start a new Table D entry for *idn.
*/
    tab = 'D';
    nd = igetntbi( lun, &tab, sizeof ( tab ) );
    cadn30( idn, adn, sizeof( adn ) ); 
    stntbi( &nd, lun, adn, nemo, cseq, sizeof( adn ), 8, 55 );

/*    
**  Now, go through the list of child descriptors corresponding to *idn.
*/
    imxcd = igetprm( "MAXCD", 5 );

    for ( i = 0; i < *ncdesc; i++ ) {
	cadn30( &cdesc[i], adn, sizeof( adn ) ); 
	if ( adn[0] == '3' ) {
/*
**	    cdesc[i] is itself a Table D descriptor, so locate it within the
**	    master table D and then store the contents within the internal
**	    Table D via a recursive call to this same routine.
*/
	    nummtb( &cdesc[i], &tab, &ipt );
	    if ( naf > 0 ) {
/*
**		There are associated fields in effect which will modify this
**		descriptor when storing it within the internal Table D.  So 
**		create a new sequence to store the contents of this descriptor
**		along with its associated fields.
*/
		rpidn = igettdi( lun );

		sprintf( rpseq, "REPLICATION SEQUENCE %.3lu",
			 ( unsigned long ) ++(*irepct) );
		memset( &rpseq[24], (int) cblk, 31 );
		sprintf( nemo2, "RPSEQ%.3lu", ( unsigned long ) *irepct );

		stseq( lun, irepct, &rpidn, nemo2, rpseq,
		    &MSTABS_BASE(idefxy)[icvidx(&ipt,&i0,&imxcd)],
		    &MSTABS_BASE(ndelem)[ipt] );
		pkint = rpidn;

	    }
	    else {
/*
**		Store cdesc[i] as is directly within the internal Table D.
*/
		stseq( lun, irepct, &cdesc[i], &MSTABS_BASE(cdmnem)[ipt][0],
		    &MSTABS_BASE(cdseq)[ipt][0],
		    &MSTABS_BASE(idefxy)[icvidx(&ipt,&i0,&imxcd)],
		    &MSTABS_BASE(ndelem)[ipt] );
		pkint = cdesc[i];
	    }
        }
	else if ( adn[0] == '2' ) {
/*
**	    cdesc[i] is an operator descriptor.
*/
	    strnum( &adn[1], &ix, 2 );
	    strnum( &adn[3], &iy, 3 );

	    if ( ( ( ix >= 4 ) && ( ix <= 6 ) ) || ( imrkopr( adn, 6 ) ) ) {
/*
**		This is a 204YYY, 205YYY, 206YYY operator, or else a 223255,
**		224255, 225255 or 232255 marker operator.  In any case,
**		generate a Table B mnemonic to hold the corresponding data.
*/
		strncpy( nemo2, adn, 6 );
		memset( &nemo2[6], (int) cblk, 2 );

		if ( ( ix == 4 ) && ( iy == 0 ) ) {
/*
**		    Cancel the most-recently added associated field.
*/
		    if ( naf-- <= 0 ) {
			sprintf( errstr, "BUFRLIB: STSEQ - TOO MANY ASSOCIATED"
			    " FIELD CANCELLATION OPERATORS" );
			bort( errstr, ( f77int ) strlen( errstr ) );
		    }
		}
		else {
/*
**		  Is nemo2 already listed as an entry within the internal
**		  Table B?
*/
		  nemtab( lun, nemo2, &pkint, &tab, &iret, 8, sizeof( tab ) );
		  if ( ( iret == 0 ) || ( tab != 'B' ) ) {
/*
**		    No, so create and store a new Table B entry for nemo2.
*/
		    tab = 'B';
		    nb = igetntbi( lun, &tab, sizeof( tab ) );

		    if ( ix == 4 ) {
			sprintf( rpseq, "Associated field of %3lu bits",
			     ( unsigned long ) iy );
			nbits = iy;
			strcpy( units, "NUMERIC" );
		    }
		    else if ( ix == 5 ) {
			sprintf( rpseq, "Text string of %3lu bytes",
			     ( unsigned long ) iy );
			nbits = iy*8;
			strcpy( units, "CCITT IA5" );
		    }
		    else if ( ix == 6 ) {
			sprintf( rpseq, "Local descriptor of %3lu bits",
			     ( unsigned long ) iy );
			nbits = iy;
			if ( nbits > 32 ) {
			    strcpy( units, "CCITT IA5" );
			}
			else {
			    strcpy( units, "NUMERIC" );
			}
		    }
		    else {   // 2-XX-255 marker operator
			adn[6] = '\0';
			if ( ix == 23 ) {
			    sprintf( rpseq, "Substituted value" );
			}
			else if ( ix == 24 ) {
			    sprintf( rpseq, "First-order statistical value" );
			}
			else if ( ix == 25 ) {
			    sprintf( rpseq, "Difference statistical value" );
			}
			else if ( ix == 32 ) {
			    sprintf( rpseq, "Replaced/retained value" );
			}
			/* For now, set a default bit width and units. */
			nbits = 8;
			strcpy( units, "NUMERIC" );
		    }
		    ilen = ( f77int ) strlen( rpseq );
		    memset( &rpseq[ilen], (int) cblk, 55 - ilen );
/*
**		    Note that 49152 = 3*(2**14), so subtracting 49152 in the
**		    following statement changes a Table D bitwise FXY value into 
**		    a Table B bitwise FXY value.
*/
		    pkint = ( igettdi( lun ) - 49152 );
		    cadn30( &pkint, adn2, sizeof( adn2 ) );

		    stntbi( &nb, lun, adn2, nemo2, rpseq,
			    sizeof( adn2 ), 8, 55 );

		    /* Initialize card to all blanks. */
		    memset( card, (int) cblk, sizeof( card ) );

		    strncpy( &card[2], nemo2, 8 );
		    strncpy( &card[16], "0", 1 );
		    strncpy( &card[30], "0", 1 );
		    sprintf( &card[33], "%4lu", ( unsigned long ) nbits );
		    strncpy( &card[40], units, strlen( units ) );
		    elemdx( card, lun, sizeof( card ) );
		  }
		  if ( ix == 4 )  {
/*
**		    Add an associated field.
*/
		    if ( naf >= MXNAF ) {
			sprintf( errstr, "BUFRLIB: STSEQ - TOO MANY ASSOCIATED"
			    " FIELDS ARE IN EFFECT AT THE SAME TIME" );
			bort( errstr, ( f77int ) strlen( errstr ) );
		    }
		    iafpk[naf++] = pkint;
		  }
		}
		if ( ix == 6 ) {
/*
**		    Skip over the local descriptor placeholder.
*/
		    if ( ++i >= *ncdesc ) {
			sprintf( errstr, "BUFRLIB: STSEQ - COULD NOT FIND LOCAL"
			    " DESCRIPTOR PLACEHOLDER FOR %s", adn );
			bort( errstr, ( f77int ) strlen( errstr ) );
		    }
		}
	    }
	    else {
		pkint = cdesc[i];
	    }
        }
	else if ( adn[0] == '1' ) {
/*
**	    cdesc[i] is a replication descriptor, so create a sequence
**	    consisting of the set of replicated descriptors and then immediately
**	    store that sequence within the internal Table D via a recursive call
**	    to this same routine.
*/
	    adn[6] = '\0';

	    strnum( &adn[3], &iy, 3 );
/*
**	    See subroutine BFRINI and COMMON /REPTAB/ for the source of the FXY
**	    values referenced in the following block.  Note we are guaranteed
**	    that 0 <= iy <= 255 since adn was generated using subroutine CADN30.
*/
	    if ( iy == 0 ) {        /* delayed replication */
		if ( ( i+1 ) >= *ncdesc ) {
		    sprintf( errstr, "BUFRLIB: STSEQ - COULD NOT FIND DELAYED "
			     "DESCRIPTOR REPLICATION FACTOR FOR %s", adn );
		    bort( errstr, ( f77int ) strlen( errstr ) );
		}
		else if ( cdesc[i+1] == ifxy( "031002", 6 ) ) {
		    pkint = ifxy( "360001", 6 );
		}
		else if ( cdesc[i+1] == ifxy( "031001", 6 ) ) {
		    pkint = ifxy( "360002", 6 );
		}
		else if ( cdesc[i+1] == ifxy( "031000", 6 ) ) {
		    pkint = ifxy( "360004", 6 );
		}
		else {
		    sprintf( errstr, "BUFRLIB: STSEQ - UNKNOWN DELAYED "
			     "DESCRIPTOR REPLICATION FACTOR FOR %s", adn );
		    bort( errstr, ( f77int ) strlen( errstr ) );
		}
		i += 2;
	    }
	    else {        /* regular replication */
		pkint = ifxy( "101000", 6 ) + iy;
		i++;
	    }
/*
**	    Store this replication descriptor within the table D entry for
**	    this parent.
*/
	    pktdd( &nd, lun, &pkint, &iret ); 
	    if ( iret < 0 ) {
		strncpy( nemo2, nemo, 8 );
		nemo2[8] = '\0';
		sprintf( errstr, "BUFRLIB: STSEQ - BAD RETURN FROM PKTDD WHEN "
			 "STORING REPLICATOR FOR PARENT MNEMONIC %s", nemo2 );
		bort( errstr, ( f77int ) strlen( errstr ) );
	    }

	    strnum( &adn[1], &ix, 2 );
/*
**	    Note we are guaranteed that 0 < ix <= 63 since adn was generated
**	    using subroutine CADN30.
*/
	    if ( ix > ( *ncdesc - i ) ) {
		sprintf( errstr, "BUFRLIB: STSEQ - NOT ENOUGH REMAINING CHILD "
			 "DESCRIPTORS TO COMPLETE REPLICATION FOR %s", adn );
		bort( errstr, ( f77int ) strlen( errstr ) );
	    }
	    else if ( ( ix == 1 ) && ( cdesc[i] >= ifxy ( "300000", 6 ) ) ) {
/*
**		The only thing being replicated is a single Table D descriptor,
**		so there's no need to invent a new sequence for this replication
**		(this is a special case!)
*/
		nummtb( &cdesc[i], &tab, &ipt );
	    	stseq( lun, irepct, &cdesc[i], &MSTABS_BASE(cdmnem)[ipt][0],
		       &MSTABS_BASE(cdseq)[ipt][0],
		       &MSTABS_BASE(idefxy)[icvidx(&ipt,&i0,&imxcd)],
		       &MSTABS_BASE(ndelem)[ipt] );
		pkint = cdesc[i];
	    }
	    else {
/*
**		Store the ix descriptors to be replicated in a local list, then
**		get an FXY value to use with this list and generate a unique
**		mnemonic and description as well.
*/

#ifdef DYNAMIC_ALLOCATION
		if ( ( rpdesc = malloc( imxcd * sizeof(f77int) ) ) == NULL ) {
		    sprintf( errstr, "BUFRLIB: STSEQ - UNABLE TO ALLOCATE SPACE"
			    " FOR RPDESC" );
		    bort( errstr, ( f77int ) strlen( errstr ) );
		}
#endif
		for ( j = 0; j < ix; j++ ) {
		    rpdesc[j] = cdesc[i+j];
		}

		rpidn = igettdi( lun );

		sprintf( rpseq, "REPLICATION SEQUENCE %.3lu",
			 ( unsigned long ) ++(*irepct) );
		memset( &rpseq[24], (int) cblk, 31 );
		sprintf( nemo2, "RPSEQ%.3lu", ( unsigned long ) *irepct );

		stseq( lun, irepct, &rpidn, nemo2, rpseq, rpdesc, &ix );

#ifdef DYNAMIC_ALLOCATION
		free( rpdesc );
#endif
		pkint = rpidn;
		i += ix - 1; 
	    }
        }
	else {
/*
**	    cdesc[i] is a Table B descriptor.
**
**	    Is cdesc[i] already listed as an entry in the internal Table B?
*/
	    numtbd( lun, &cdesc[i], nemo2, &tab, &iret, sizeof( nemo2 ),
		    sizeof( tab ) );
	    if ( ( iret == 0 ) || ( tab != 'B' ) ) {
/*
**		No, so search for it within the master table B.
*/
		nummtb( &cdesc[i], &tab, &ipt );
/*
** 		Start a new Table B entry for cdesc[i].
*/
		nb = igetntbi( lun, &tab, sizeof( tab ) );
		cadn30( &cdesc[i], adn2, sizeof( adn2 ) ); 
		stntbi( &nb, lun, adn2, &MSTABS_BASE(cbmnem)[ipt][0],
			&MSTABS_BASE(cbelem)[ipt][0], sizeof( adn2 ), 8, 55 );

		/* Initialize card to all blanks. */
		memset( card, (int) cblk, sizeof( card ) );

		strncpy( &card[2], &MSTABS_BASE(cbmnem)[ipt][0], 8 );
		strncpy( &card[13], &MSTABS_BASE(cbscl)[ipt][0], 4 );
		strncpy( &card[19], &MSTABS_BASE(cbsref)[ipt][0], 12 );
		strncpy( &card[33], &MSTABS_BASE(cbbw)[ipt][0], 4 );
		strncpy( &card[40], &MSTABS_BASE(cbunit)[ipt][0], 14 );
		elemdx( card, lun, sizeof( card ) );
	    }
	    pkint = cdesc[i];
        }
	if ( strncmp( adn, "204", 3 ) != 0 ) {
/*
**	    Store this child descriptor within the table D entry for this
**	    parent, preceding it with any associated fields that are currently
**	    in effect.
**
**	    Note that associated fields are only applied to Table B descriptors,
**	    except for those in Class 31.
*/
	    if ( ( naf > 0 ) && ( pkint < ifxy( "100000", 6 ) ) &&
		    ( ( pkint < ifxy( "031000", 6 ) ) ||
		      ( pkint > ifxy( "031255", 6 ) ) )  ) {
	        for ( j = 0; j < naf; j++ ) {
		    pktdd( &nd, lun, &iafpk[j], &iret );
		    if ( iret < 0 ) {
		      sprintf( errstr, "BUFRLIB: STSEQ - BAD RETURN FROM PKTDD "
			     "WHEN STORING ASSOCIATED FIELDS" );
		      bort( errstr, ( f77int ) strlen( errstr ) );
		    }
		}
	    }
/*
**	    Store the child descriptor.
*/
	    pktdd( &nd, lun, &pkint, &iret ); 
	    if ( iret < 0 ) {
		strncpy( nemo2, nemo, 8 );
		nemo2[8] = '\0';
		sprintf( errstr, "BUFRLIB: STSEQ - BAD RETURN FROM PKTDD WHEN "
		     "STORING CHILD FOR PARENT MNEMONIC %s", nemo2 );
		bort( errstr, ( f77int ) strlen( errstr ) );
            }
	}
    }
}
