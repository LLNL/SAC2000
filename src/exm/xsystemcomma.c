#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ xsystemcommand(nerr)
int *nerr;
{
	char kmsg[MCMSG+1];
	int ncmsg;
	void zsysop();

	/*=====================================================================
	 * PURPOSE:  To execute the parameter-setting command SYSTEMCOMMAND.
	 *           This command allows the user to execute a system command
	 *           without exiting from SAC.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCMSG
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CERR, LCREST, ZSYSOP
	 *=====================================================================
	 * ASSUMPTIONS:
	 * - Operating system can support such a feature.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870211:  Changed due to modification of LCREST.
	 *    820817:  Changed to newest set of parsing and checking functions.
	 *    820115:  Cleaned up and documented.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870211
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Store rest of command line in output message buffer. */

	if( !lcrest( MCMSG, kmsg,MCMSG+1, &ncmsg ) ){
		cerr( 1001 );
		goto L_8888;
		}

	/* EXECUTION PHASE: */

	/* - Execute the system command. */

	zsysop( kmsg,MCMSG+1, &ncmsg, nerr );

L_8888:
	return;

} /* end of function */

