#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/cpf.h"
void /*FUNCTION*/ getmacroinfo(macrolevel, varsname, varsname_s)
int *macrolevel;
char *varsname;   int varsname_s;
{



	/*=====================================================================
	 * PURPOSE: To get information about the current macro be executed.
	 *=====================================================================
	 * OUTPUT VARIABLES:
	 *   macrolevel:   Nesting level of macro currently being executed. [i]
	 *   varsname:     Name of vars list containing macro information. [c]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    
	 *    cpf:     nmacrolevel, kvarsname
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900207
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Return current macro level and name of vars list containing
	 *   information about macro state. */
	*macrolevel = cmcpf.nmacrolevel;
	fstrncpy( varsname, varsname_s-1, kmcpf.kvarsname, strlen(kmcpf.kvarsname));

L_8888:
	return;

} /* end of function */

