#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
void /*FUNCTION*/ splitvname(vars, vars_s, ncvars, ic1, ic2)
char *vars;   int vars_s;
int ncvars, *ic1, *ic2;
{
	int icdelim, icsubd;



	/*=====================================================================
	 * PURPOSE: To split a vars name into the "parent" and "child" portions.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:   Name of vars list. [c]
	 *    ncvars: Number of characters in vars. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ic1:   Ending character of "parent" vars list. [i]
	 *           This is set to zero if there is no "parent."
	 *           If non zero, the parent name is vars(1:ic1).
	 *    ic2:   Starting character of "child" vars list. [i]
	 *=====================================================================
	 * MODULE/LEVEL:   vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:  KSUBDL
	 *    vars:  VLISTDELIM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:   indexa
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890630:  Changed argument list. Added ncvars; deleted ic3.
	 *    890327:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890327
	 *===================================================================== */
	/* - Determine where last vars list delimiter is (if any) in name. */
	icdelim = indexa( vars,vars_s, kmvars.vlistdelim, FALSE, TRUE );

	/* - Determine where last subdirectory delimiter is (if any) in name. */

	icsubd = indexa( vars,vars_s, KSUBDL, FALSE, TRUE );

	/* - A parent exists if the list delimiter occurs after the subdirectory delimiter. */

	if( icdelim > icsubd ){
		*ic1 = icdelim - 1;
		*ic2 = icdelim + 1;

		/* - Otherwise no parent exists and the child starts after the subdirectory
		 *   delimiter.  This still works even if there is also no subdirectory delimiter
		 *   because "indexa" returns a 0, making ic2 = 1. */

		}
	else{
		*ic1 = 0;
		*ic2 = icsubd + 1;
		}

L_8888:
	return;
} /* end of function */

