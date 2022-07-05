#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ iniwidth()
{
	int j, j_;



	/*=====================================================================
	 * PURPOSE:  To initialize the "standard" width list.
	 *           This is the list of line-widths used in plotting data.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    IIWIDTH:   List of "standard" widths.
	 *    NIWIDTH:   Length of IIWIDTH.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    GEM:
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Set up standard width list. */
	cmgem.niwidth = 10;
	for( j = 1; j <= cmgem.niwidth; j++ ){
		j_ = j - 1;
		Iiwidth[j] = j;
		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    920526:  Original version from INICOL.
	 *===================================================================== */

} /* end of function */

