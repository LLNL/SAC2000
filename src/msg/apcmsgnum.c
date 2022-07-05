#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ apcmsgnum(number)
int number;
{
	char kmsg[MCMSG+1];

	/*=====================================================================
	 * PURPOSE:  To append a numbered message to current message.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    number:  Message number. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  msg/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG
	 *=====================================================================
	 * GLOBAL COUPLING:  See setmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900430:  Changed name from apnmsg to apcmsgnum.
	 *    860203:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900430
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Get message from disk file. */
	getsmsg( number, kmsg,MCMSG+1 );

	/* - Append to current message. */

	apcmsg( kmsg,MCMSG+1 );

L_8888:
	return;

} /* end of function */

