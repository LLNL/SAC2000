#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/exm.h"
#include "../../inc/bom.h"
void /*FUNCTION*/ xboec(nerr)
int *nerr;
{
	int index;



	/*=====================================================================
	 * PURPOSE:  To execute the parameter-setting command BOEC.
	 *           This command controls certain error conditions that
	 *           that can occur during binary operations.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:   BOM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    EXM:     KECTP, NECTP
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    BOM:     KECNPT, KECDEL
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LKLIST
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "NPTS condition":  set control for unequal number of data points. */
		if( lklist( "N$",3, (char*)kmexm.kectp,9, cmexm.nectp, &index ) ){
			strcpy( kmbom.kecnpt, kmexm.kectp[index - 1] );

			/* "DELTA condition":  set control for sampling interval mismatch. */
			}
		else if( lklist( "D$",3, (char*)kmexm.kectp,9, cmexm.nectp, 
		 &index ) ){
			strcpy( kmbom.kecdel, kmexm.kectp[index - 1] );

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;

		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820817:  Changed to newest set of parsing and checking functions.
	 *    810723:  Original version.
	 *===================================================================== */

} /* end of function */

