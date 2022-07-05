#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ allocatevnode(node, nerr)
int *node, *nerr;
{
	int node_;



	/*=====================================================================
	 * PURPOSE:  To allocate an unused vars node from vars storage system.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    node:    The unused node number. [i]
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    numvars, varsindex, MAXVARS, MAXVARSEXCEEDED
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    numvars
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  setmsg, apimsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881114:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881114
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	*node = 0;

	/* - Check to see if an existing node has been released. */

	for( *node = 1; *node <= cmvars.numvars; (*node)++ ){
		node_ = *node - 1;
		if( Varsindex[*node] <= 0 )
			goto L_8888;
		}

	/* - Increment the number of nodes if there is room.
	 *   Raise error condition if there is no room. */

	if( cmvars.numvars <= MAXVARS ){
		cmvars.numvars = cmvars.numvars + 1;
		*node = cmvars.numvars;
		}
	else{
		*nerr = MAXVARSEXCEEDED;
		setmsg( "ERROR", *nerr );
		apimsg( MAXVARS );
		}

L_8888:
	return;

} /* end of function */

