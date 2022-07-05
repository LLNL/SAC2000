#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ inilin(iilin, nilin)
int iilin[], *nilin;
{
	int j, j_;

	int *const Iilin = &iilin[0] - 1;


	/* Ind
	 *=====================================================================
	 * PURPOSE:  To initialize the "standard" linestyle list.
	 *           This is the list of linestyles used in plotting data.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    IILIN:   List of "standard" linestyles.
	 *    NILIN:   Length of IILIN.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Set up standard linestyle table. */
	*nilin = 4;
	for( j = 1; j <= *nilin; j++ ){
		j_ = j - 1;
		Iilin[j] = j;
		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    830115:  Original version.
	 *===================================================================== */

} /* end of function */

