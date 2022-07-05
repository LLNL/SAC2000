#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ inicol(iicol, nicol)
int iicol[], *nicol;
{
	int j, j_;

	int *const Iicol = &iicol[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To initialize the "standard" color list.
	 *           This is the list of colors used in plotting data.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    IICOL:   List of "standard" colors.
	 *    NICOL:   Length of IICOL.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Set up standard color table. */
	*nicol = 6;
	for( j = 1; j <= *nicol; j++ ){
		j_ = j - 1;
		Iicol[j] = j;
		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    821221:  Original version from INIGEM.
	 *===================================================================== */

} /* end of function */

