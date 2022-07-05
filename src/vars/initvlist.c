#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ initvlist(vars, vars_s, index, nerr)
char *vars;   int vars_s;
int *index, *nerr;
{
	int node;
	void decodevdesc();



	/*=====================================================================
	 * PURPOSE:  To initialize a vars list for sequential access.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:    Name of vars list. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    index:   Index of initial entry in vars list. [i]
	 *             This value is passed to the sequential access subroutine.
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    varsindex, VARSLISTNOTFOUND
	 *    mem:     sacmem
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    desclength, namelength, valuelength,
	 *             deleteflag, readonlyflag, indirectflag, valuename
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  indexb, existsvlist, decodevdesc, setmsg, apcmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881114:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881114
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Convert vars list name to a node number and return starting index. */

	if( existsvlist( vars,vars_s, "BOTH", &node ) ){
		*index = Varsindex[node];
                cmvars.varsnode1 = *index;
		decodevdesc( cmmem.sacmem[*index], &cmvars.deleteflag, &cmvars.readonlyflag, 
		 &cmvars.indirectflag, &cmvars.sharedflag, &cmvars.reservedflag, 
		 &cmvars.applflag1, &cmvars.applflag2, &cmvars.valuetype, 
		 &cmvars.desclength, &cmvars.namelength, &cmvars.valuelength );
		*index = *index + cmvars.desclength + cmvars.namelength;
		}
	else{
		*index = 0;
		*nerr = VARSLISTNOTFOUND;
		setmsg( "ERROR", *nerr );
		apcmsg( vars,vars_s );
		goto L_8888;
		}

L_8888:
	return;

} /* end of function */

