#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
int /*FUNCTION*/ lnxtcl(kcl, kcl_s, index1, index2)
char *kcl;   int kcl_s;
int *index1, *index2;
{
	int lnxtcl_v;
	byte kdel;
	int idel, ncl;
	void *_p0;
        char *strtemp, *cstart;

	/*=====================================================================
	 * PURPOSE:  To get the next entry from a character list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KCL:     Character list. [c]
	 *    INDEX1:  Index pointing to the first character of old entry. [i]
	 *    INDEX2:  Index pointing to the last character of old entry. [i]
	 *=====================================================================
	 * FUNCTION VALUE:  .TRUE. if an entry was found, .FALSE. otherwise.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    INDEX1:  Index pointing to the first character of new entry. [i]
	 *    INDEX2:  Index pointing to the last character of new entry. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXA
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NCL:     Number of characters in KCL. [i]
	 *    KDEL:    Delimiter between entries. [c1]
	 *    IDEL:    Used to search for delimiter in character list. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900119:  Was not determing length of character list each time.
	 *    870929:  Was not determining delimiter each time.
	 *    860918:  Changed initialization logic.
	 *    860128:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860128
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine delimiter and length of character list. */
	kdel = kcl[0];
	ncl = (kcl_s - 1);

	/* - Set start pointer if first pass.
	 *   Increment start pointer if not first pass. */

	if( *index1 <= 0 ){
		*index1 = 2;
		}
	else{
		*index1 = *index2 + 2;
		}

	/* - Find next occurance of delimiter. */
/*
        strtemp = malloc(ncl - *index1 + 2);
        strncpy(strtemp,kcl+*index1 - 1,ncl - *index1 + 1);
        strtemp[ncl - *index1 + 1] = '\0';

        cstart = strtemp;
*/
        cstart = kcl+*index1-1;

L_1000:

	idel = indexa( cstart, ncl-*index1+2, kdel, TRUE, TRUE );

	/* - See if we have an entry or not. */

	if( idel > 1 ){
		lnxtcl_v = TRUE;
		*index2 = *index1 + idel - 2;
		}
	else if( idel == 1 ){
		lnxtcl_v = TRUE;
		*index1 = *index1 + 1;
                cstart++;
		goto L_1000;
		}
	else{
		lnxtcl_v = FALSE;
		*index2 = 0;
		}

L_8888:
/*        free(strtemp); */
	return( lnxtcl_v );

} /* end of function */

