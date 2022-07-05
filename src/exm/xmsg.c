#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
void /*FUNCTION*/ xmsg(nerr)
int *nerr;
{
	char kmsg[MCMSG+1];
	int ncmsg;



	/*=====================================================================
	 * PURPOSE:  To execute the action command MESSAGE.
	 *          This command sends a message to the user's terminal.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001
	 *=====================================================================
	 * MODULE/LEVEL: exm/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG, MUNOUT
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, cfmt, cresp, lcchar, 
	 *             setmsg, apcmsg, outmsg, clrmsg
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "message":  message to write to the user's terminal. */
		if( lcchar( MCMSG, kmsg,MCMSG+1, &ncmsg ) ){
			setmsg( "OUTPUT", 99 );
			apcmsg( kmsg,MCMSG+1 );
			outmsg();
			clrmsg();

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

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820809:  Changed to newest set of parsing and checking functions.
	 *    820115:  Cleaned up logic and documented subroutine.
	 *    810120:  Changed to output message retrieval from disk.
	 *    800605:  Original version.
	 *===================================================================== */

} /* end of function */

