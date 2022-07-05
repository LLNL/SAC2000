#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/msg.h"
void aplmsg(char *kalpha, int kalpha_s)
{
	int isave, nalpha;



	/*=====================================================================
	 * PURPOSE:  To append a new line of text to current message.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kalpha:  Alphanumeric string to begin new line with. [c]
	 *=====================================================================
	 * MODULE/LEVEL:  exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    msg:     MLIMSG, nlimsg, nchmsg, autoout
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    msg:     nlimsg, nchmsg, klimsg
	 *=====================================================================
	 * GLOBAL COUPLING:  See SETMSG.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  indexb, outmsg, clrmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890104:  Added automatic output mode logic.
	 *    830916:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890104
	 *===================================================================== */
	/* PROCEDURE: */
	/* - If buffer is full and in automatic output mode, 
	 *   send message, clear message buffer. */
	if( cmmsg.nlimsg == MLIMSG && cmmsg.autoout ){
		outmsg();
		isave = cmmsg.itpmsg;
		clrmsg();
		cmmsg.itpmsg = isave;

		/* - Otherwise increment buffer line counter if there is room. */

		}
	else if( cmmsg.nlimsg < MLIMSG ){
		cmmsg.nlimsg = cmmsg.nlimsg + 1;
		}

	/* - Determine length of text string without trailing blanks. */

	nalpha = indexb( kalpha,kalpha_s );

	/* - Start new line of message with string.  Include one trailing blank. */

	fstrncpy( kmmsg.klimsg[cmmsg.nlimsg - 1], MCMSG, kalpha,
                 max( 1,nalpha ));
	cmmsg.nchmsg = nalpha + 1;

L_8888:
	return;

} /* end of function */

