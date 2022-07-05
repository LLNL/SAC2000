#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
void /*FUNCTION*/ setnfiles(nfiles)
int nfiles;
{



	/*=====================================================================
	 * PURPOSE: To set the number of files in the data file list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    nfiles:  Number of files in data file list. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  dfm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MDFL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    dfm:     ndfl
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900305:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900305
	 *===================================================================== */
	/* PROCEDURE: */
	cmdfm.ndfl = min( nfiles, MDFL );

L_8888:
	return;

} /* end of function */

