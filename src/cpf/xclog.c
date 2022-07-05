#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ xclog(log, nerr)
int *log;
int *nerr;
{

	/*=====================================================================
	 * PURPOSE: To parse a simple on/off type command.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    log:     Set to .TRUE. if "ON" token found. [l]
	 *             Set to .FALSE. if "OFF" token found.
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lcmore, lclog, cfmt, cresp
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820505:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900510
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- Turn logical flag on/off. */
		if( lclog( log ) ){

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;
		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0.
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
		goto L_8888;

L_8888:
	return;

} /* end of function */

