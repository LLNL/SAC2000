#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int lcmore(int* nerr)
{
	int lcmore_v;



	/*=====================================================================
	 * PURPOSE:  To tell whether there are more tokens in current command.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    LCMORE:  Set to .TRUE. if there are more tokens in command
	 *             and no parsing error has been encountered. [l]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error number if an error has occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom, ncom, lcerr
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820420:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Set function value to .TRUE. if:
	 *   (1) There are more tokens in current command  AND
	 *   (2) No parsing error has occurred since last call */
	lcmore_v = cmcom.jcom <= cmcom.ncom && cmcom.ncerr == 0;

	/* - Also return the command error number. */

	*nerr = cmcom.ncerr;

L_8888:
	return( lcmore_v );

} /* end of function */

