#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/msg.h"
void /*FUNCTION*/ apfmsg(float_)
double float_;
{
	char kalpha[17];


	/*=====================================================================
	 * PURPOSE:  To append a floating point number to current message.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    FLOAT:   Floating point number. [f]
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
	 *    SACLIB:  LJUST, APCMSG
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Convert integer to alphanumeric. */
        sprintf(kalpha,"%16.5g",float_);
	ljust( kalpha,17 );

	/* - Append alphanumeric representation to message. */

	apcmsg( kalpha,17 );

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    850805:  Changed conversion to float to a WRITE.
	 *    830916:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  830916
	 *===================================================================== */

} /* end of function */

