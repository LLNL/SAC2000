#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ setvreadonly(vars, vars_s, name, name_s, roflag, 
	 nerr)
char *vars;   int vars_s;
char *name;   int name_s;
int *roflag;
int *nerr;
{
	int indexvar;
	float type;
	void encodevdesc();

        float *Sacmem;

	/*=====================================================================
	 * PURPOSE:  To set the readonly flag for a vars entry.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:       Name of vars list. [c]
	 *    name:       Name of vars entry. [c]
	 *    roflag:     Set to .TRUE. if var is to be readonly. [l]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:       Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    datablock, valuetype, desclength, namelength, valuelength,
	 *             VARNOTFOUND
	 *    mem:     sacmem
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  existsv, encodevdesc
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861229:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  861229
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - If variable exists, set the read-only flag.
	 *   Otherwise, report error. */

	if( existsv( vars,vars_s, name,name_s ) ){
/* this routine used to use an uninitialized value of indexvar to index into Sacmem. */
/* changed when doing memory management to use index of current node as set through  */
/* call to existsv.                       2/7/94 ldm                                 */
                Sacmem = cmmem.sacmem[Varsindex[cmvars.varsnode]];
		encodevdesc( Sacmem, &cmvars.deleteflag, roflag, 
		 &cmvars.indirectflag, &cmvars.sharedflag, &cmvars.reservedflag, 
		 &cmvars.applflag1, &cmvars.applflag2, &type, &cmvars.desclength, 
		 &cmvars.namelength, &cmvars.valuelength );
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

