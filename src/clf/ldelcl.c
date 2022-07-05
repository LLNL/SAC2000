#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
int /*FUNCTION*/ ldelcl(kcl, kcl_s, kentry, kentry_s)
char *kcl;   int kcl_s;
char *kentry;   int kentry_s;
{
	int ldelcl_v;
	byte kdel;
	int index1, index2, j1, j2, ncl, nentry;

	/*====================================================================
	 * PURPOSE:  To delete an ENTRY in a character list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KCL:     Character list. [cl]
	 *    KENTRY:  ENTRY to delete. [c]
	 *             It is NOT an error if ENTRY is not in character list.
	 *=====================================================================
	 * FUNCTION VALUE:  .TRUE. if KENTRY was found and deleted,
	 *                  .FALSE. if not found.
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
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870429:  Corrected an indexing bug.
	 *    860918:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860918
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Assume the entry will not be found. */
	ldelcl_v = FALSE;

	/* - Initialize character pointer and determine length of entry. */

	index1 = 0;

	nentry = indexb( kentry,kentry_s );

	/* - Loop on each ENTRY in character list. */

	while ( lnxtcl( kcl,kcl_s, &index1, &index2 ) ){

		/* -- If match, delete characters in list, fill with delimiter, and set
		 *    return value to .TRUE. */

		if( memcmp(kentry,kcl+index1 - 1,max(nentry,index2-index1+1)) == 0 ){
			ncl = (kcl_s - 1);
			for( j1 = index2 + 2; j1 <= ncl; j1++ ){
				j2 = j1 - nentry - 1;
				kcl[j2 - 1] = kcl[j1 - 1];
			}
			for( j1 = j2 + 1; j1 <= ncl; j1++ ){
				kcl[j1 - 1] = kcl[0];
			}
			ldelcl_v = TRUE;

			break ;
		}
	}

L_8888:
	return( ldelcl_v );

} /* end of function */

