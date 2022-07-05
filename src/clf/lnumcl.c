#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
int /*FUNCTION*/ lnumcl(kcl, kcl_s, num, index1, index2)
char *kcl;   int kcl_s;
int num, *index1, *index2;
{
	int lnumcl_v;
	int j;

	/*=====================================================================
	 * PURPOSE:  To get a specific entry from a character list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KCL:     Character list. [c]
	 *    NUM:     Number of entry you want to get. [i]
	 *             (To get the fourth entry in list, set NUM to 4).
	 *=====================================================================
	 * FUNCTION VALUE:  .TRUE. if an entry was found, .FALSE. otherwise.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    INDEX1:  Index pointing to the first character in entry. [i]
	 *    INDEX2:  Index pointing to the last character in entry. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * EXAMPLES OF USE:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LNXTCL
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Initialize pointer. */
	*index1 = 0;

	/* - Loop until we have the specific entry or list is exhausted. */

	j = 1;
L_1000:
	if( lnxtcl( kcl,kcl_s, index1, index2 ) ){
		if( j == num ){
			lnumcl_v = TRUE;
			}
		else{
			j = j + 1;
			goto L_1000;
			}
		}
	else{
		lnumcl_v = FALSE;
		}

L_8888:
	return( lnumcl_v );

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    860917:  Changed initialization method.
	 *    860128:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860128
	 *===================================================================== */

} /* end of function */

