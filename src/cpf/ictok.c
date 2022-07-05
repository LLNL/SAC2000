#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
void /*FUNCTION*/ ictok(inc)
int inc;
{



	/*=====================================================================
	 * PURPOSE:  To increment to current command token counter.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    inc:     Token increment value.
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
	 * With one exception, all "lcxxx" and "lkxxx" command parsing functions 
	 * do their own token incrementing.  The exception is "lctok" which
	 * merely returns the next token.  After that token has been success-
	 * fully parsed, this subroutine should be called with a increment of +1.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    821008:  Fixed bug involving range check on JCOM.
	 *    810901:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900510
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Increment token counter. */
	cmcom.jcom = cmcom.jcom + inc;

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

