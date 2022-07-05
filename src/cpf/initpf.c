#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#define DOINITS
#include "../../inc/tpf.h"
#undef DOINITS
void /*FUNCTION*/ initpf()
{
	int j, j_;



	/*=====================================================================
	 * PURPOSE: Variable initialization of common block CMTPF.
	 *=====================================================================
	 * PARAMETERS:
	 *=====================================================================
	 * MODULE/LEVEL:
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * VARIABLE DEFINITIONS:
	 *===================================================================== */
	/* PROCEDURE: */
	cmtpf.jtok1 = 0;
	kmtpf.kbl1 = ' ';
	for( j = 1; j <= MCPS; j++ ){
		j_ = j - 1;
		Ktok1[j] = ' ';
		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    810414:  Original version.
	 *===================================================================== */

} /* end of function */

