#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lcreal(realv)
float *realv;
{
	int lcreal_v;



	/*=====================================================================
	 * PURPOSE: To parse a "real variable" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lcreal:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    realv:   Real variable found in command. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom, ncom, kcom, itypcm, inumbr, flnum
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     jcom
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820423:  Adjustments due to new command parsing system.
	 *    810207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Get real variable from next symbol. */
	if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
		*realv = Flnum[cmcom.jcom];
		cmcom.jcom = cmcom.jcom + 1;
		lcreal_v = TRUE;
		}
	else{
		lcreal_v = FALSE;
		}

L_8888:
	return( lcreal_v );

} /* end of function */

