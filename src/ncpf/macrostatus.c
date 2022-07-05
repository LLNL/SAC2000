#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/cpf.h"
int /*FUNCTION*/ macrostatus()
{
	int macrostatus_v;



	/*=====================================================================
	 * PURPOSE: To get the status of the current macro being executed.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *   macrostatus:  Set to .TRUE. if everything is ok.
	 *                 Set to .FALSE. if an error has occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    
	 *    cpf:     lmacrostatus
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900207:  Changed status variable to a logical variable.
	 *    900129:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900129
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Return status variable as a logical function value. */
	macrostatus_v = cmcpf.lmacrostatus;

L_8888:
	return( macrostatus_v );

} /* end of function */

