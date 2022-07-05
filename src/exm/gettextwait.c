#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/exm.h"
void /*FUNCTION*/ gettextwait(mode, mode_s)
char *mode;   int mode_s;
{


	/*=====================================================================
	 * PURPOSE: To get the current text output wait mode.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *     mode:   Wait mode option. [c]
	 *             = "ON" to cause SAC to pause and wait for user response
	 *                after a screen full of output has been generated.
	 *             = "OFF" to cause SAC not to pause.
	 *=====================================================================
	 * MODULE/LEVEL:   exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    exm:    ktextwait
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900410:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900410
	 *===================================================================== */
	/* PROCEDURE: */
	fstrncpy( mode, mode_s-1, kmexm.ktextwait, strlen(kmexm.ktextwait));

L_8888:
	return;

} /* end of function */

