#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
int /*FUNCTION*/ existsv(vars, vars_s, name, name_s)
char *vars;   int vars_s;
char *name;   int name_s;
{
	int existsv_v;
	int _l0, ncname, node, nvname;
	void decodevdesc(), zgetc();

        float *Sacmem;

	/*=====================================================================
	 * PURPOSE:  To test for the existence of a vars entry.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:    Name of vars list. [c]
	 *    name:    Name of vars entry. [c]
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    existsv: Set to .TRUE. if the entry was found in the vars list.
	 *             Set to .FALSE. if not found or vars list not found.
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    varsindex, VALUENIL
	 *    mem:     sacmem
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    descindex, desclength, namelength, valuelength,
	 *             deleteflag, readonlyflag, indirectflag, valuename
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  indexb, existsvlist, decodevdesc, zgetc
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890706:  Added logic to treat empty vars entry name.
	 *    870831:  Fixed bug when checking "name" versus "valuename."
	 *    861229:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861229
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Convert vars list name to a node number and get starting index. */
	if( existsvlist( vars,vars_s, "BOTH", &node ) ){
		cmvars.descindex = Varsindex[node];
		decodevdesc( cmmem.sacmem[cmvars.descindex], &cmvars.deleteflag, 
		 &cmvars.readonlyflag, &cmvars.indirectflag, &cmvars.sharedflag, 
		 &cmvars.reservedflag, &cmvars.applflag1, &cmvars.applflag2, 
		 &cmvars.valuetype, &cmvars.desclength, &cmvars.namelength, 
		 &cmvars.valuelength );
		cmvars.descindex = cmvars.descindex + cmvars.desclength + 
		 cmvars.namelength;
		}
	else{
		existsv_v = FALSE;
		goto L_8888;
		}

	/* - Search for a vars variable with the requested name.
	 *   Save descriptor information in vars common block variables.
	 *   If variable is not found, common points to VARSTEMINATOR. */

	ncname = indexb( name,name_s );
L_1000:
        Sacmem = cmmem.sacmem[Varsindex[node]]+(cmvars.descindex-Varsindex[node]);
	decodevdesc( Sacmem, &cmvars.deleteflag, &cmvars.readonlyflag, 
	 &cmvars.indirectflag, &cmvars.sharedflag, &cmvars.reservedflag, 
	 &cmvars.applflag1, &cmvars.applflag2, &cmvars.valuetype, &cmvars.desclength, 
	 &cmvars.namelength, &cmvars.valuelength );
	fstrncpy( kmvars.valuename, MAXCVNAME, " ", 1 );

        Sacmem += cmvars.desclength;
	zgetc( Sacmem, kmvars.valuename, 4*cmvars.namelength );
	nvname = indexb( kmvars.valuename,MAXCVNAME+1 );
	if( cmvars.valuetype == VALUENIL ){
		existsv_v = FALSE;
		}
	else if( ncname != nvname ){
		cmvars.descindex = cmvars.descindex + cmvars.desclength + 
		 cmvars.namelength + cmvars.valuelength;
		goto L_1000;
		}
	else if( memcmp(name,kmvars.valuename,min(nvname,MAXCVNAME)) == 0 ){
		existsv_v = TRUE;
		}
	else{
		cmvars.descindex = cmvars.descindex + cmvars.desclength + 
		 cmvars.namelength + cmvars.valuelength;
		goto L_1000;
		}

L_8888:
	return( existsv_v );

} /* end of function */

