#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/eam.h"
void /*FUNCTION*/ pkfunc(fdold, fdnew, chfsta, chflta, chf)
double fdold, fdnew;
float *chfsta, *chflta, *chf;
{
	float fdfd;



	/*=====================================================================
	 * PURPOSE: To calculate characteristic function and its averages
	 *          by automatic picker.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    FDOLD:   Old (last) value of filtered trace. [f]
	 *    FDNEW:   New (current) value of filtered trace. [f]
	 *    CHFSTA:  Old short term average of characteristic function. [f]
	 *    CHFLTA:  Old int term average of characteristic function. [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    CHFSTA:  New short term average. [f]
	 *    CHFLTA:  New int term average. [f]
	 *    CHF:     New value of characteristic function. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  EAM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    EAM:     C2, C3, C5,
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Compute first difference of filtered data. */
	fdfd = fdnew - fdold;

	/* - Compute characteristic function. */

	*chf = powi(fdnew,2) + cmeam.c2*powi(fdfd,2);

	/* - Compute the short term average. */

	*chfsta = *chfsta + cmeam.c3*(*chf - *chfsta);

	/* - Compute the int term average. */

	*chflta = *chflta + cmeam.c4*(*chf - *chflta);

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    801101:  Factored from PK1.
	 *===================================================================== */

} /* end of function */

