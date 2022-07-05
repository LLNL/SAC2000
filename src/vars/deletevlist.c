#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ deletevlist(vars, vars_s, mode, nerr)
char *vars;   int vars_s;
char *mode;
int *nerr;
{
	char savename[MCPFN+1];
	int _l0, jnode, jnode_, ncsave, node;
	void zsysop();
        char *cattemp;

	/*=====================================================================
	 * PURPOSE:  To delete a vars list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:    Name of vars list. [c]
	 *    mode:    Delete mode. [c]
	 *             = 'MEMORY' to only delete list in memory.
	 *             = 'BOTH' to delete list in memory and on disk.
	 *             Case insensitive and only first character is needed.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCPFN
	 *    vars:    varsname, ncvarsname, varsindex, numvars
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    mem:     isacmem
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  existsvlist, relamb, releasevnode, zsysop
	 *=====================================================================
	 * KNOWN BUGS:
	 * - Does not delete the entry in the parent list.
	 * - Deletion of disk images is done using UNIX system command "rm".
	 *   This is NOT portable and should be replaced with calls to "zfiles"
	 *   to get all files in directory and "wild" to perform pattern
	 *   matching, and "zdest" to actually destroy the matched files.  
	 *   This would make the delete machine independent.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890727:  Was not deleting sublists or disk images.
	 *    870407:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890727
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - If vars name is found in list of currently active vars lists: */

	if( existsvlist( vars,vars_s, mode, &node ) ){

		/* -- Save name for use in deleting children. */
		strcpy( savename, kmvars.varsname[node - 1] );
		ncsave = Ncvarsname[node];

		/* -- Release memory block and vars node. */
		relamb( cmmem.sacmem, Varsindex[node], nerr );
		if( *nerr != 0 )
			goto L_8888;
		releasevnode( node, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Loop on each vars list in memory.
		 *    Delete it from memory if it beints to the same vars family. */
		for( jnode = 1; jnode <= cmvars.numvars; jnode++ ){
			jnode_ = jnode - 1;
			if( memcmp(savename,kmvars.varsname[jnode_],min(ncsave,MAXCVNAME)) == 0 ){
				relamb( cmmem.sacmem, Varsindex[jnode], nerr );
				if( *nerr != 0 )
					goto L_8888;
				releasevnode( jnode, nerr );
				if( *nerr != 0 )
					goto L_8888;
				}
			}

		/* -- Delete disk images if requested. */
		if( mode[0] == 'B' || mode[0] == 'b' ){
                        cattemp = malloc(3+ncsave+1+1);
                        strcpy(cattemp,"rm ");
                        strncat(cattemp,savename,ncsave);
                        strcat(cattemp,"*");
                        _l0 = ncsave+4;
			zsysop( cattemp, 3+ncsave+1+1, &_l0, nerr );
                        free(cattemp);
			if( *nerr != 0 )
				goto L_8888;
			}

		}

L_8888:
	return;

} /* end of function */

