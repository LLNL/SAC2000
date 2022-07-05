#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "complex.h"
#include "proto.h"

void /*FUNCTION*/ cnvfre(kcard, kcard_s, mentry, nentry, fentry, ientry, 
	 kalpha, kalpha_s, lstrict, nerr)
char *kcard;   int kcard_s;
int mentry, *nentry;
float fentry[];
int ientry[];
char *kalpha;   int kalpha_s;
int lstrict, *nerr;	/* lstrict added. maf 970129 */
{
	int ic, ic1, ic2, itype, nc;
        char *strtemp;


	float *const Fentry = &fentry[0] - 1;
	int *const Ientry = &ientry[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To convert a free format alphanumeric data card into
	 *           a floating point array.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KCARD:    Alphanumeric data card. [c]
	 *    MENTRY:   Maximum number of floating point fields to
	 *              convert.  Size of FENTRY array. [i]
	 *    IENTRY:   Indicates whether to decode this entry or not.
	 *              = -2, save alphanumeric field
	 *              = -1, skip over
	 *              >= 0, decode
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NENTRY:  Number of floating point fields converted. [i]
	 *    FENTRY:  Array of floating point fields. [ra]
	 *    NERR:    Error return flag.  Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  POPTOK, SETMSG, APCMSG, APIMSG, CNVATF
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970129:  Added lstrict to indicate how to deal with errors of
	 *             a string of digits that is to int to be converted to
	 *             an int.  1 means catch the error and warn the user;
	 *             0 means let it slide unnoticed.  maf 
	 *    860910:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  860910
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Determine length of input card. */

	nc = indexb( kcard,kcard_s );

	/* - Initialize pointer to current character in string and
	 *   number of output values. */

	ic = 0;
	*nentry = 0;

	/* - Loop on each token in string.
	 *   Terminates when string is exhausted or maximum number
	 *   of output entries is reached. */

	poptok( kcard, nc, &ic, &ic1, &ic2, &itype );

	/* -- Loop until there are no more tokens in string or */
	/*    until there is no more room in output list. */

	while ( itype != 0 && *nentry < mentry ) {
	    /* -- Convert this token to a floating point variable. */
	    *nentry = *nentry + 1;
	    if( Ientry[*nentry] >= 0 ){
		strtemp = malloc( ic2 - ic1 + 2 ) ;
		strncpy ( strtemp , kcard + ic1 - 1 , ic2 - ic1 + 1 ) ;
		strtemp[ ic2 - ic1 + 1 ] = '\0' ;

		cnvatf( strtemp , ic2-ic1+2 , &Fentry[*nentry], lstrict, nerr );

		free ( strtemp ) ;
	    }
	    else if ( Ientry[ *nentry ] == -2 ){
		fstrncpy( kalpha, kalpha_s-1, kcard+ic1 - 1, ic2 - ic1 + 1);
	    }
	    if( *nerr )
		break ;

	    poptok( kcard, nc, &ic, &ic1, &ic2, &itype ) ;
	}

	if ( *nentry >= mentry ) {
	    *nerr = 1358;
	    setmsg( "ERROR", *nerr );
	    apimsg( mentry );
	}

} /* end of function */

