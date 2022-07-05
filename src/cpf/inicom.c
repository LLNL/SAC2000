#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
void /*FUNCTION*/ inicom()
{



	/*=====================================================================
	 * PURPOSE: Variable initialization of common block CMCOM.
	 *=====================================================================
	 * MODULE/LEVEL:  COM/4
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * PARAMETERS:
	 *=====================================================================
	 * VARIABLE DEFINITIONS:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820420:  Added NCERR.
	 *    810414:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	cmcom.inumbr = 1;
	cmcom.ialpha = 2;
	cmcom.icont = 3;
	cmcom.ncom = 0;
	cmcom.jcom = 0;
        kmcom.nkargs = 0;
        kmcom.nkargs_allocated = 0;

	cmcom.ncerr = 0;

L_8888:
	return;

} /* end of function */

