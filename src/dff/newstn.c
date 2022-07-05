#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/snf.h"
void /*FUNCTION*/ newstn(ksninp, ksnout, ksnout_s)
char *ksninp, *ksnout;   int ksnout_s;
{
	int j, j_;

	/* Ind
	 *=====================================================================
	 * PURPOSE: To convert an old station name to its current name.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KSNINP:  Input station name; only first 4 characters examined.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KSNOUT:  Output station name.
	 *=====================================================================
	 * MODULE/LEVEL:
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Loop through list of old station names:
	 * - If KSNINP is found, set KSNOUT to the new station name.
	 * - If not found, set KSNOUT to KSNINP. */
	for( j = 1; j <= MSN; j++ ){
		j_ = j - 1;
		if( memcmp(ksninp,kmsnf.ksnold[j_],4) == 0 ){
			fstrncpy( ksnout, ksnout_s-1, kmsnf.ksnnew[j_],
                                               strlen(kmsnf.ksnnew[j_]));
			return;
			}
		}
	fstrncpy( ksnout, ksnout_s-1, ksninp, strlen(ksninp));
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    800605: Original version [Prime].
	 *    800922: Added four DS2 stations to list.
	 *    810514: Changed common block organization.
	 *===================================================================== */

} /* end of function */
/* GLOBAL COUPLING:
 *===================================================================== */
