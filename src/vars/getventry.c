#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ getventry(vars, vars_s, name, name_s, type, length, 
	 index, nerr)
char *vars;   int vars_s;
char *name;   int name_s;
int type, *length, *index, *nerr;
{
	char varssub[MAXCVNAME+1];
	int nc1, nc2, node;


	/*=====================================================================
	 * PURPOSE:  To get a vars entry from a vars list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:       Name of vars list. [c]
	 *    name:       Name of vars entry. [c]
	 *    type:       Type of vars entry. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    length:     Number of values in vars. [i]
	 *    index:      SACMEM index of beginning of vars. [i]
	 *                First word address of vars is sacmem(index).
	 *    nerr:       Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    MAXCVNAME, valuetype, valuelength, indirectflag, descindex,
	 *             desclength, namelength, BADVALUETYPE, VARNOTFOUND,
	 *             varsindex, varslength, vlistdelim
	 *    mem:     isacmem
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  existsv, existsvlist, setmsg, apcmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881109:  Added logic for indirect data block option.
	 *    861229:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861229
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - If variable exists and is the right type, return length and index.
	 *   Otherwise, report error. */

	if( existsv( vars,vars_s, name,name_s ) ){
		if( cmvars.valuetype == type ){
			if( cmvars.indirectflag ){
				nc1 = indexb( vars,vars_s );
				nc2 = indexb( name,name_s );
                                fstrncpy( varssub, MAXCVNAME, vars+0,nc1);
                                fstrncpy( varssub+nc1, MAXCVNAME-nc1, (char *)&kmvars.vlistdelim, 1);
                                fstrncpy( varssub+nc1+1, MAXCVNAME-nc1-1, name, nc2);

				if( existsvlist( varssub,MAXCVNAME+1, "BOTH", &node ) ){
					*index = Varsindex[node];
					*length = Varslength[node];
					}
				else{
					*nerr = VARNOTFOUND;
					setmsg( "ERROR", *nerr );
					apcmsg( vars,vars_s );
					apcmsg( name,name_s );
					apcmsg( "(Indirect data block missing.)",31 );
					}
				}
			else{
				*index = cmvars.descindex + cmvars.desclength + cmvars.namelength;
				*length = cmvars.valuelength;
				}
			}
		else{
			*nerr = BADVALUETYPE;
			setmsg( "ERROR", *nerr );
			apcmsg( vars,vars_s );
			apcmsg( name,name_s );
			}
		}
	else{
		*nerr = VARNOTFOUND;
		setmsg( "ERROR", *nerr );
		apcmsg( vars,vars_s );
		apcmsg( name,name_s );
		}

L_8888:

	return;

} /* end of function */

