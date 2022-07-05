#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/exm.h"
void /*FUNCTION*/ xpause(nerr)
int *nerr;
{
	char kret[9];
	int nc;
	float fperio;
	void zgtmsg(), zsleep(), zwprmt();


	/*=====================================================================
	 * PURPOSE: To parse the parameter-setting command PAUSE.
	 *          This command sends a message to the terminal and then
	 *          pauses and waits for a return message.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCMSG
	 *    EXM:     KPAUSE
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    EXM:     KPAUSE
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCCHAR, ZGTMSG
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

		/* -- "PERIOD ON|OFF|v":  set period of time to pause. */
		if( lklogr( "PERIOD$",8, &cmexm.lperio, &fperio ) ){
			cmexm.nperio = (int)( 1000.0*fperio );
			if( cmexm.nperio <= 0 )
				cmexm.lperio = FALSE;
		}

		/* -- Determine text of pause message. */
		else if( lkchar( "MESSAG$",8, MCMSG - 2, kmexm.kpause,MCMSG+1, 
		 &nc ) ){
			subscpy( kmexm.kpause, nc, -1, MCMSG, " $" );
		}

		/* -- Bad syntax. */
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();
		}
	}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	/* EXECUTION PHASE: */

	if( cmexm.lperio ){
		nc = indexb( kmexm.kpause,MCMSG+1 );
		if( nc > 2 )
			zwprmt( kmexm.kpause,MCMSG+1, nc - 2 );
		zsleep( &cmexm.nperio );
	}
	else{
		zgtmsg( kmexm.kpause,MCMSG+1, kret,9 );
	}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    860925:  Added PERIOD option.
	 *    840206:  Original version.
	 *===================================================================== */

} /* end of function */

