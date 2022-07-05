#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/msg.h"
void clrmsg(void)
{
	int j, j_;



	/*=====================================================================
	 * PURPOSE:  To clear current message condition.
	 *=====================================================================
	 * MODULE/LEVEL:  msg/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    msg:     nummsg, itpmsg, nlimsg, klimsg
	 *=====================================================================
	 * GLOBAL COUPLING:  See "setmsg".
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    830916:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860203
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Reset message number and message length. */
	cmmsg.nummsg = 0;
	cmmsg.itpmsg = 0;

	for( j = 1; j <= cmmsg.nlimsg; j++ ){
		j_ = j - 1;
		fstrncpy( kmmsg.klimsg[j_], MCMSG, " ", 1);
		}
	cmmsg.nlimsg = 1;
	cmmsg.nchmsg = 0;

L_8888:
	return;

} /* end of function */

