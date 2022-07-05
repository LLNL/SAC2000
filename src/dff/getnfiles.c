#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
void /*FUNCTION*/ getnfiles(nfiles)
int *nfiles;
{



	/*=====================================================================
	 * PURPOSE: To get the number of files in the data file list.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nfiles:  Number of files in data file list. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  dfm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    dfm:     ndfl
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900305:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900305
	 *===================================================================== */
	/* PROCEDURE: */
	*nfiles = cmdfm.ndfl;

L_8888:
	return;

} /* end of function */

