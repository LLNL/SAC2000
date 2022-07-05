#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ releasevnode(node, nerr)
int node, *nerr;
{



	/*=====================================================================
	 * PURPOSE:  To release a vars node back to the vars storage system.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    node:    The node number. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    varsname, ncvarsname, varsindex, varsmodified,
	 *             varsindiret, varsnilindex
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881114:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881114
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Clear the vars node entry. */

	Varsindex[node] = -1;
	fstrncpy( kmvars.varsname[node - 1], MAXCVNAME, " ", 1);
	Ncvarsname[node] = 0;
	Varsmodified[node] = FALSE;
	Varsindirect[node] = FALSE;
	Varsnilindex[node] = 0;

L_8888:
	return;

} /* end of function */

