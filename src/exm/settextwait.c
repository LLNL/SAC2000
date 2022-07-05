#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/exm.h"
void /*FUNCTION*/ settextwait(mode)
char *mode;
{
	char test[3];



	/*=====================================================================
	 * PURPOSE: To set the text output wait mode.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *     mode:   Wait mode option. [c]
	 *             = "ON" to cause SAC to pause and wait for user response
	 *                after a screen full of output has been generated.
	 *             = "OFF" to cause SAC not to pause.
	 *=====================================================================
	 * MODULE/LEVEL:   exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    exm:    ktextwait
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900410:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900410
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Convert first two input characters to upper case. */
	modcase( TRUE, mode, 2, test );

	/* - Test versus allowed options. */

	if( strcmp(test,"ON") == 0 ){
		strcpy( kmexm.ktextwait, "ON      " );
		}
	else if( strcmp(test,"OF") == 0 ){
		strcpy( kmexm.ktextwait, "OFF     " );
		}
	else{
		strcpy( kmexm.ktextwait, "ON      " );
		}

L_8888:
	return;

} /* end of function */

