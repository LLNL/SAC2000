#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ deletev(vars, vars_s, name, name_s, nerr)
char *vars;   int vars_s;
char *name;   int name_s;
int *nerr;
{
	char fullvars[MAXCVNAME+1], varssub[MAXCVNAME+1];
	int indirectsave;
	int index, ncfullvars, ncname, nilindex, niloffset, node, 
	 numcopy, numdelete, varindex;


	/*=====================================================================
	 * PURPOSE:  To delete a vars entry.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:       Name of vars list. [c]
	 *    name:       Name of vars entry. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:       Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    deleteflag, readonlyflag, indirectflag, desclength, 
	 *             namelength, valuelength, VARNODELETE, varsindex, vlistdelim
	 *    mem:     isacmem, sacmem 
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    varsmodified
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  existsv, existsvlist, copy, relamb, releasevnode, encodevnil
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900207:  Was not setting modified flag when deleting variable.
	 *    890329:  Fixed bug in deleting a variable with an indirect block.
	 *    870107:  Added logic to remove indirect data block if necessary.
	 *    861229:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861229
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - If variable exists and is deletable, delete it.
	 *   Otherwise, report error. */

	if( existsv( vars,vars_s, name,name_s ) ){

		indirectsave = cmvars.indirectflag;
		if( !cmvars.deleteflag ){

			/* -- Determine how many words to delete. */
/*			varindex = cmvars.descindex; */
			varindex = cmvars.descindex - Varsindex[cmvars.varsnode];
			numdelete = cmvars.desclength + cmvars.namelength + cmvars.valuelength;

			/* -- Delete words by moving remainder of vars list up. */
/*			numcopy = Varsnilindex[cmvars.varsnode] - varindex - numdelete; */
			numcopy = Varsnilindex[cmvars.varsnode] - (varindex+Varsindex[cmvars.varsnode]) - numdelete;
			copy( (int*)cmmem.sacmem[Varsindex[cmvars.varsnode]]+varindex+numdelete,
                              (int*)cmmem.sacmem[Varsindex[cmvars.varsnode]]+varindex, numcopy );

			/* -- Encode a new nil terminator. */
			niloffset = Varsnilindex[cmvars.varsnode] - Varsindex[cmvars.varsnode] - 
			 numdelete;
			encodevnil( cmvars.varsnode, niloffset, nerr );
			if( *nerr != 0 )
				goto L_8888;

			/* -- Set modified flag. */
			Varsmodified[cmvars.varsnode] = TRUE;

			/* -- Release indirect block and its vars node if necessary. */
			if( indirectsave ){
				convlistname( vars,vars_s, fullvars,MAXCVNAME+1, &ncfullvars, 
				 nerr );
				if( *nerr != 0 )
					goto L_8888;
				ncname = indexb( name,name_s );

                                fstrncpy( varssub, MAXCVNAME, fullvars, min(ncfullvars,MAXCVNAME));
                                fstrncpy( varssub+min(ncfullvars,MAXCVNAME), MAXCVNAME-min(ncfullvars,MAXCVNAME),
                                                       (char *)&kmvars.vlistdelim, 1);
                                fstrncpy( varssub+min(ncfullvars,MAXCVNAME)+1, MAXCVNAME-min(ncfullvars,MAXCVNAME)-1,
                                                                        name, ncname);

				if( existsvlist( varssub,MAXCVNAME+1, "MEMORY", &node ) ){
					index = Varsindex[node];
					releasevnode( node, nerr );
					if( *nerr != 0 )
						goto L_8888;
					relamb( cmmem.sacmem, index, nerr );
					if( *nerr != 0 )
						goto L_8888;
					}
				}

			}
		else{
			*nerr = VARNODELETE;
			setmsg( "ERROR", *nerr );
			apcmsg( vars,vars_s );
			apcmsg( name,name_s );
			}
		}

L_8888:

	return;

} /* end of function */

