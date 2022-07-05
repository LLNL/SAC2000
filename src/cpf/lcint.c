#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lcint(intv)
int *intv;
{
	int lcint_v;



	/*=====================================================================
	 * PURPOSE: To parse a "integer variable" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    LCINT:   .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    intv:    Integer variable found in command. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom, ncom, kcom, itypcm, inumbr, ialpha, flnum
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     jcom
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820423:  Adjustments due to new command parsing system.
	 *    820415:  Added information hiding logic.
	 *    810207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - See if next token is numeric. */
	lcint_v = cmcom.jcom <= cmcom.ncom && Itypcm[cmcom.jcom] == cmcom.inumbr;

	/* - If so, convert to integer. */

	if( lcint_v ){
		*intv = (int)( Flnum[cmcom.jcom] + 0.1 );
		cmcom.jcom = cmcom.jcom + 1;
		}

L_8888:
	return( lcint_v );

} /* end of function */

