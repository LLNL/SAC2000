#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/bbs.h"
void /*FUNCTION*/ createbbs(nerr)
int *nerr;
{
	int notused;

	/*=====================================================================
	 * PURPOSE:  To create the "blackboard store."
	 *=====================================================================
	 * MODULE/LEVEL: bbs/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    
	 *    bbs:     knmbbs, nlnbbs
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  createvlist
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870301:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870301
	 *===================================================================== */
	/* PROCEDURE: */
	createvlist( kmbbs.knmbbs,MCPFN+1, cmbbs.nlnbbs, &notused, nerr );

L_8888:
	return;

} /* end of function */

