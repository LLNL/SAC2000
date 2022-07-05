#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/msg.h"
void /*FUNCTION*/ autooutmsg(lmode)
int lmode;
{



	/*=====================================================================
	 * PURPOSE:  To set the message type.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    lmode:  Automatic output mode flag. [l]
	 *            = .TRUE.  to automatically send and clear internal
	 *                      output message when buffer is full.
	 *            = .FALSE. to disable automatic mode.
	 *                      (Must use outmsg and clrmsg when disabled.)
	 *=====================================================================
	 * MODULE/LEVEL:  msg/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    msg:     autoout
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  outmsg, clrmsg
	 *=====================================================================
	 * GLOBAL COUPLING: See documentation for setmsg.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890104:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890104
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Save new value of automatic output mode flag. */
	cmmsg.autoout = lmode;

	/* - If terminating automatic output, send and clear buffer of remaining message. */

	if( !cmmsg.autoout ){
		outmsg();
		clrmsg();
		}

L_8888:
	return;

} /* end of function */

