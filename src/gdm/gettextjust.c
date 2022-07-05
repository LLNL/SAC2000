#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gdm.h"
void /*FUNCTION*/ gettextjust(khorz, khorz_s, kvert)
char *khorz;   int khorz_s;
char *kvert;
{



	/*=====================================================================
	 * PURPOSE:  To inquire about the current graphics text justification.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    khorz:   Horizontal text justification. [c]
	 *             = 'LEFT' for left justification.
	 *             = 'CENTER' for centered justification.
	 *             = 'RIGHT' for right justification.
	 *    kvert:   Vertical text justification. [c]
	 *             = 'BOTTOM' for bottom justification.
	 *             = 'CENTER' for centered justification.
	 *             = 'TOP' for TOP justification.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     ihjust, ivjust
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861017:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861017
	 *===================================================================== */
	/* PROCEDURE: */
	if( cmgdm.ihjust == 1 ){
		fstrncpy( khorz, khorz_s-1, "LEFT", 4 );
	}
	else if( cmgdm.ihjust == 2 ){
		fstrncpy( khorz, khorz_s-1, "CENTER", 6 );
	}
	else{
		fstrncpy( khorz, khorz_s-1, "RIGHT", 5 );
	}

	if( cmgdm.ivjust == 1 ){
		fstrncpy( khorz, khorz_s-1, "BOTTOM", 6 );
	}
	else if( cmgdm.ivjust == 2 ){
		fstrncpy( khorz, khorz_s-1, "CENTER", 6 );
	}
	else{
		fstrncpy( khorz, khorz_s-1, "TOP", 3 );
	}

L_8888:
	return;

} /* end of function */

