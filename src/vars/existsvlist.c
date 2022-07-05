#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
int /*FUNCTION*/ existsvlist(vars, vars_s, mode, node)
char *vars;   int vars_s;
char *mode;
int *node;
{
	char fullvars[MAXCVNAME+1];
	int  idx ;
	int existsvlist_v;
	int ncfullvars, ncheck, ncnode, ncvars, nerr, node_;



	/*=====================================================================
	 * PURPOSE:  To test for the existence of a vars list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:    Name of vars list. [c]
	 *    mode:    Search mode. [c]
	 *             = 'MEMORY' to only check for existence in memory.
	 *             = 'BOTH' to check first in memory and then on disk.
	 *             Case insensitive and only first character is needed.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    existsv: Set to .TRUE. if the entry was found in the vars list.
	 *             Set to .FALSE. if not found or vars list not found.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    node:    The number of the vars node found. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    numvars, MAXCVNAME, varsname, varsindex
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    descindex, varsnode
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  convlistname, indexb, readvfile, setmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900511:  Fixed bug involving not properly setting "varsnode"
	 *             when a vars list was read from disk.
	 *    890727:  Set node to -1 on return when no list was found.
	 *    890302:  Moved conversion of vars list name to a subroutine.
	 *    881114:  Changed name from existsvsect to existsvlist.
	 *             Added "mode" input option.
	 *    881108:  Added current node and automatic disk reading logic.
	 *    870904:  Changed number of characters used in name matching.
	 *    861229:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881108
	 *===================================================================== */
	/* PROCEDURE: */
	nerr = 0;
	existsvlist_v = FALSE;
	*node = -1;
	cmvars.varsnode = -1;

	for( idx = 0 ; idx < MAXCVNAME ; idx++ )
            fullvars[ idx ] = ' ' ;
	fullvars[ MAXCVNAME ] = '\0' ;

	/* - Convert relative vars name to absolute vars name. */

	convlistname( vars,vars_s, fullvars,MAXCVNAME+1, &ncfullvars, &nerr );
	if( nerr != 0 )
		goto L_8888;

	/* - Search for vars name in active memory list.
	 *   Return node number if found in memory list.  */

	for( *node = 1; *node <= cmvars.numvars; (*node)++ ){
		node_ = *node - 1;
		ncnode = Ncvarsname[*node];
		ncheck = max( ncfullvars - 1, ncnode );
		if( memcmp(fullvars+1,kmvars.varsname[node_],min(ncheck,MAXCVNAME)) == 0 ){
			existsvlist_v = TRUE;
			cmvars.descindex = Varsindex[*node];
			cmvars.varsnode = *node;
			goto L_8888;
			}
		}

	/* - If
 mode is "BOTH" and vars list is not in memory, try to read it from disk.
	 *   Return with FALSE condition if not in memory or on disk. */

	if( mode[0] == 'B' || mode[0] == 'b' ){
		readvfile( fullvars,MAXCVNAME+1, node, &nerr );
		if( nerr == 0 ){
			existsvlist_v = TRUE;
			cmvars.descindex = Varsindex[*node];
			cmvars.varsnode = *node;
			}
		}

L_8888:
	return( existsvlist_v );

} /* end of function */

