#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
int /*FUNCTION*/ nfndcl(kcl, kcl_s, kentry, kentry_s, index1, index2)
char *kcl;   int kcl_s;
char *kentry;   int kentry_s;
int *index1, *index2;
{
	byte kdel;
	int jentry, nentry, nfndcl_v;

	/*====================================================================
	 * PURPOSE:  To find an entry in a character list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KCL:     Character list. [cl]
	 *    KENTRY:    Entry to search for. [c]
	 *=====================================================================
	 * FUNCTION VALUE:  The index of the entry in the list if the
	 *                  the entry was found, 0 if not found. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    INDEX1:  Pointer to the first character in entry. [i]
	 *    INDEX2:  Pointer to the last character in entry. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * EXAMPLES OF USE:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXA, LNXTCL
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NCL:     Number of characters in KCL. [i]
	 *    KDEL:    Delimiter between entries. [c1]
	 *    IDEL:    Used to search for delimiter in character list. [i]
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Assume the worst. */
	nfndcl_v = 0;

	/* - Initialize character pointer and determine length of entry. */

	*index1 = 0;
	nentry = indexb( kentry,kentry_s );

	/* - Loop on each token in character list. */

	jentry = 1;
L_1000:
	if( lnxtcl( kcl,kcl_s, index1, index2 ) ){

		/* -- If match, set index and return. */
		if( memcmp(kentry,kcl+*index1 - 1,max(nentry,*index2 - *index1 + 1)) 
		         == 0 ){
			nfndcl_v = jentry;
			goto L_8888;

			/* -- Otherwise, increment counter and loop. */
			}
		else{
			jentry = jentry + 1;
			goto L_1000;
			}

		}

L_8888:
	return( nfndcl_v );

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    860918:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860918
	 *===================================================================== */

} /* end of function */

