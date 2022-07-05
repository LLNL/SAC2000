#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ inivars()
{
	int node, node_;


	/*=====================================================================
	 * PURPOSE:  To initialize the VARS storage subsystem.
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    MAXVARS
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    vabsflag, vlistdelim, varsname, varsindex, varsmodified,
	 *             varsindirect, varsnilindex, numvars, currentnode
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881114:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881114
	 *===================================================================== */
	/* PROCEDURE: */
	kmvars.vabsflag = '#';
	kmvars.vlistdelim = '.';

	for( node = 1; node <= MAXVARS; node++ ){
		node_ = node - 1;
		Varsindex[node] = -1;
		fstrncpy( kmvars.varsname[node_], MAXCVNAME, " ", 1 );
		Ncvarsname[node] = 0;
		Varsmodified[node] = FALSE;
		Varsindirect[node] = FALSE;
		Varsnilindex[node] = 0;
		}

	cmvars.numvars = 0;
	cmvars.currentnode = 0;

        if((vfilelist.filelist = (struct varsfile *)malloc(NVFILELIST*sizeof(struct varsfile)))
                                           != NULL )    {
           vfilelist.nallocated = NVFILELIST;
           vfilelist.nentries   = 0;

        }else {
           fprintf(MUNOUT,"error initializing varsfile list--quitting\n");
           exit(1);
	}

L_8888:
	return;

} /* end of function */

