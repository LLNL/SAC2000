#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/msg.h"
void /*FUNCTION*/ apimsg(integr)
int integr;
{
	char kalpha[9];



	/*=====================================================================
	 * PURPOSE:  To append an integer to current message.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    INTEGR:  Integer. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    MSG:     NLIMSG, NCHMSG
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    MSG:     NLIMSG, NCHMSG, KLIMSG
	 *=====================================================================
	 * GLOBAL COUPLING:  See SETMSG.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  CNVITA, LJUST, APCMSG
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Convert integer to alphanumeric. */
	cnvita( integr, kalpha,9 );
	ljust( kalpha,9 );

	/* - Append alphanumeric representation to message. */

	apcmsg( kalpha,9 );

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    830916:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  830916
	 *===================================================================== */

} /* end of function */

