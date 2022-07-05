#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ ddttm(ndttm1, ndttm2, diff)
int ndttm1[], ndttm2[];
float *diff;
{
	int nday;

	int *const Ndttm1 = &ndttm1[0] - 1;
	int *const Ndttm2 = &ndttm2[0] - 1;


	/* Ind
	 *=====================================================================
	 * PURPOSE: To compute difference in seconds between two time arrays.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * GLOBAL SYSTEM INPUT:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * ASSUMPTIONS:
	 *=====================================================================
	 * LIMITATIONS:
	 *=====================================================================
	 * KNOWN ERRORS:
	 *=====================================================================
	 * EXTERNAL DEPENDENCIES:
	 *===================================================================== */
	/* PROCEDURE: */
	if( 4*(Ndttm2[1]/4) == Ndttm2[1] )
		nday = 366;
	else
		nday = 365;

	*diff =     0.001 * (float) ( Ndttm1[ 6 ] - Ndttm2[ 6 ] ) + 
			    (float) ( Ndttm1[ 5 ] - Ndttm2[ 5 ] ) + 
		   60.000 * (float) ( Ndttm1[ 4 ] - Ndttm2[ 4 ] ) + 
		 3600.000 * (float) ( Ndttm1[ 3 ] - Ndttm2[ 3 ] ) + 
		86400.000 * (float) ( Ndttm1[ 2 ] - Ndttm2[ 2 ] ) + 
	 nday * 86400.000 * (float) ( Ndttm1[ 1 ] - Ndttm2[ 1 ] ) ;

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    970908:  ddttm no inter checks that the year and day of year 
	 *             match.  That is now left to the calling function. maf
	 *    810130:  Original version.
	 *===================================================================== */

} /* end of function */

