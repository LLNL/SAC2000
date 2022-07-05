#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/bbs.h"
void /*FUNCTION*/ inibbs()
{

	/*=====================================================================
	 * PURPOSE:  To initialze the "blackboard function" common blocks.
	 *=====================================================================
	 * MODULE/LEVEL: BBS/4
	 *=====================================================================
	 * VARIABLE DEFINITIONS
	 *    knmbbs:    Name of the blackboard store. [c]
	 *    nlnbbs:    Initial length of the blackboard store. [i]
	 *               Length is automatically expanded as needed.
	 *    kbbsinit:  Flag used to know if common has been initialized. [k]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870301:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  870301
	 *===================================================================== */
	/* PROCEDURE: */
	strcpy( kmbbs.knmbbs, "blackboard");
	cmbbs.nlnbbs = 128;
	strcpy( kmbbs.kbbsinit, "INITDONE" );

L_8888:
	return;

} /* end of function */

