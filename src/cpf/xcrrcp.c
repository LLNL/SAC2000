#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ xcrrcp(realmn, realmx, real1, real2, nerr)
double realmn, realmx;
float *real1, *real2;
int *nerr;
{

	/*=====================================================================
	 * PURPOSE: To parse real range-checked token pairs.
	 *          The second must be larger than the first.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lcmore, cfmt, cresp, lcrrc
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820608:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900510
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		if( lcrrcp( realmn, realmx, real1, real2 ) ){

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;
		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

} /* end of function */

