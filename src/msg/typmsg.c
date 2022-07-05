#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/msg.h"
void /*FUNCTION*/ typmsg(ktype)
char *ktype;
{
	char ktpinp[9];
	int j, j_;



	/*=====================================================================
	 * PURPOSE:  To set the message type.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ktype:   Type of message. [c]
	 *             = 'ERROR' for an fatal error message condition.
	 *             = 'WARNING' for a warning message condition.
	 *             = 'OUTPUT' for an informational message condition.
	 *             = 'COMMANDS' for commands typed at the terminal.
	 *             = 'MACROS' for commands executed from a macro file.
	 *             Only the first letter need be entered.
	 *=====================================================================
	 * MODULE/LEVEL:  msg/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    msg:     MTPMSG, ktpmsg
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    msg:     itpmsg
	 *=====================================================================
	 * GLOBAL COUPLING: See documentation for setmsg.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  modcase
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    ktpinp:  ktype converted to upper case for testing purposes. [k]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881230:  Deleted some obsolete coding.
	 *    860130:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881230
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine what type of message condition to raise.
	 *   If the type is not a valid one, raise an informational condition. */
	modcase( TRUE, ktype, MCPW, ktpinp );
	for( j = 1; j <= MTPMSG; j++ ){
		j_ = j - 1;
		if( ktpinp[0] == kmmsg.ktpmsg[j_][0] )
			goto L_1100;
		}
	j = 3;

L_1100:
	cmmsg.itpmsg = j;

L_8888:
	return;

} /* end of function */

