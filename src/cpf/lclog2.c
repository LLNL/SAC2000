#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
int /*FUNCTION*/ lclog2(ktrue, ktrue_s, kfalse, kfalse_s, logv)
char *ktrue;   int ktrue_s;
char *kfalse;   int kfalse_s;
int *logv;
{
	int lclog2_v;

	/*=====================================================================
	 * PURPOSE: To parse a "logical variable" command construct. The first
	 *          token turns logical flag on, the second token turns it off.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lclog:   .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ktrue:   The token to search for corresponding to .TRUE. [k]
	 *    kfalse:  The token to search for corresponding to .FALSE. [k]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    logv:    Logical variable found in command. [l]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lckey
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for the "true" token. */
	if( lckey( ktrue,ktrue_s ) ){
		*logv = TRUE;
		lclog2_v = TRUE;

		/* - Check for the "false" token. */

		}
	else if( lckey( kfalse,kfalse_s ) ){
		*logv = FALSE;
		lclog2_v = TRUE;

		/* - Neither token found. */

		}
	else{
		lclog2_v = FALSE;

		}

L_8888:
	return( lclog2_v );

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820622:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  820622
	 *===================================================================== */

} /* end of function */

