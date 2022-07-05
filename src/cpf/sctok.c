#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
void /*FUNCTION*/ sctok(num)
int num;
{



	/*=====================================================================
	 * PURPOSE:  To set the current command token counter.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    num:     The new value for the command token counter. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom, ncom
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     jcom
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * All "lcxxx" and "lkxxx" command parsing functions (except for "lctok")
	 * do their own token incrementing.
	 * This call is only needed if you need to bypass normal incrementing.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    821009:  Fixed bug involving range check of JCOM.
	 *    810823:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900510
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Set token counter. */
	cmcom.jcom = num;

	/* - Make sure new value is within allowable range. */

	if( cmcom.jcom < 1 ){
		cmcom.jcom = 1;
		}
	else if( cmcom.jcom > cmcom.ncom ){
		cmcom.jcom = cmcom.ncom + 1;
		}

L_8888:
	return;

} /* end of function */

