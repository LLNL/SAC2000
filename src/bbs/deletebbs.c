#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/bbs.h"
void /*FUNCTION*/ deletebbs(nerr)
int *nerr;
{

	/*=====================================================================
	 * PURPOSE:  To delete the "blackboard store."
	 *=====================================================================
	 * MODULE/LEVEL: bbs/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    
	 *    bbs:     knmbbs
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  deletevlist
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    880412:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  880412
	 *===================================================================== */
	/* PROCEDURE: */
	deletevlist( kmbbs.knmbbs,MCPFN+1, "MEMORY", nerr );

L_8888:
	return;

} /* end of function */

