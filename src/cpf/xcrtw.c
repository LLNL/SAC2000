#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"

void /*FUNCTION*/ xcrtw(lrtw, krtw, krtw_s, ortw, nerr)
int *lrtw;
char *krtw;   int krtw_s;
float ortw[];
int *nerr;
{

	float *const Ortw = &ortw[0] - 1;


	/*=====================================================================
	 * PURPOSE: To parse a "relative time window" command.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    lrtw:    Set to .TRUE. if RTW was turned on, .FALSE. if
	 *             turned off. Otherwise not changed. [l]
	 *    krtw:    RTW reference times [ka=2]
	 *             First is starting reference time, second stopping time.
	 *    ortw     RTW offset times [fa=2]
	 *    nerr:    Error flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lcmore, cfmt, cresp, lcrtw
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820608:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900510
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

		/* -- Check for RTW construct at current location. */
		if( lcrtw( lrtw, krtw,krtw_s, ortw ) )
		{ /* do nothing */ }

		/* -- Bad syntax. */
		else{
			*nerr = 1001;
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();
		}
	}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

} /* end of function */

