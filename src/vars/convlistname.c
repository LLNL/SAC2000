#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
void /*FUNCTION*/ convlistname(vars, vars_s, fullvars, fullvars_s, 
	 ncfullvars, nerr)
char *vars;   int vars_s;
char *fullvars;   int fullvars_s;
int *ncfullvars, *nerr;
{
	char _c0[2], _c1[2];
	int nc, ncvars;




	/*=====================================================================
	 * PURPOSE:  To convert a relative vars name to its full (absolute) name.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:      Name of relative vars list. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    fullvars:    Name of full (absolute) vars list. [c]
	 *    ncfullvars:  Number of characters in fullvars. [i]
	 *    nerr:        Error return flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    vabsflag, vlistdelim, KSUBDL
	 *    vars:    currentnode, varsname, ncvarsname, numvars
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  indexb, indexa, setmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890630:  Added ncvarsname to inc/vars and ncfullvars to arg list.
	 *    890309:  Added check for prior conversion of vars list name.
	 *    890302:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890302
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - If name has already been converted, copy old name to new and return. */

	ncvars = indexb( vars,vars_s );
	if( vars[0] == kmvars.vabsflag ){
		fstrncpy( fullvars, fullvars_s-1, vars, strlen(vars));
		*ncfullvars = ncvars;
		goto L_8888;
		}

	/* - If a blank vars list name was input, return the current list name. */

	if( ncvars <= 0 ){
		if( cmvars.currentnode > 0 ){
			nc = Ncvarsname[cmvars.currentnode];
                        fstrncpy( fullvars, fullvars_s-1, (char *)&kmvars.vabsflag, 1);
                        fstrncpy( fullvars+1, fullvars_s-1-1,
                                kmvars.varsname[cmvars.currentnode - 1]+0,min(nc,MAXCVNAME));
			*ncfullvars = 1 + nc;
			}
		else{
			*nerr = NOCURRENTLIST;
			setmsg( "ERROR", *nerr );
			goto L_8888;
			}

		/* - If a vars name was input and a current node is defined, append the
		 *   vars name to the current list name to get the new list name.
		 *   If no current node is defined, set the new list name to the vars name. */

		}
	else{
		if( cmvars.currentnode > 0 ){
                        fstrncpy( fullvars, fullvars_s-1, (char *)&kmvars.vabsflag, 1);
                        fstrncpy( fullvars+1, fullvars_s-1-1,
                                              kmvars.varsname[cmvars.currentnode - 1],
                                              min(Ncvarsname[cmvars.currentnode],MAXCVNAME));
                        fstrncpy( fullvars+1+min(Ncvarsname[cmvars.currentnode],MAXCVNAME),
                                  fullvars_s-1-1-min(Ncvarsname[cmvars.currentnode],MAXCVNAME),
                                  (char *)&kmvars.vlistdelim, 1);
                        fstrncpy( fullvars+1+min(Ncvarsname[cmvars.currentnode],MAXCVNAME)+1,
                                  fullvars_s-1-1-min(Ncvarsname[cmvars.currentnode],MAXCVNAME)-1,
                                                                             vars,ncvars);
			*ncfullvars = 1 + Ncvarsname[cmvars.currentnode] + 1 + 
			 ncvars;
			}
		else{
                        fstrncpy( fullvars, fullvars_s-1, (char *)&kmvars.vabsflag, 1);
                        fstrncpy( fullvars+1, fullvars_s-1-1, vars, ncvars);                      
			*ncfullvars = 1 + ncvars;
			}
		}

L_8888:

	return;

} /* end of function */

