#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/sss.h"
#include "../../inc/tt.h"
void /*FUNCTION*/ xphase(nerr)
int *nerr;
{
	char kttunit[9];
	int iphase, nchar;



	/* Ind
	 *=====================================================================
	 * PURPOSE:  To execute the action command PHASE.
	 *           This command saves all the phases.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  SSS/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    TT:    NPHASES, KPHASES
	 *=====================================================================
	 * MULROUTINES CALLED:
	 *    SACLIB:  LCMORE, LCCL
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	iphase = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "more": */

		/* -- "phase":  save phase */
		if( lcchar( MTTLEN, (char*)kmtt.kphases[iphase],6, &nchar ) ){
			iphase = iphase + 1;

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;

		}
	cmtt.nphases = iphase;

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    920805:  Original version.
	 *===================================================================== */

} /* end of function */

