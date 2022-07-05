#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ findvnil(startindex, nilindex)
int startindex, *nilindex;
{
	char kname[5];
	int _l0;
	void decodevdesc(), zgetc();

        float *Sacmem;

	/*=====================================================================
	 * PURPOSE:  To find the terminating NIL of a vars list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    startindex:  SACMEM array starting location. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nilindex:    SACMEM index of the terminating NIL entry.
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    VALUENIL
	 *    mem:     sacmem
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    descindex, desclength, namelength, valuelength, valuetype,
	 *             deleteflag, readonlyflag, datablockflag, valuename
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  decodevdesc
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861229:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  861229
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Search for terminating NIL vars entry. */
	cmvars.descindex = startindex;

        Sacmem = cmmem.sacmem[cmvars.varsnode1]+(startindex-cmvars.varsnode1);

L_1000:
	decodevdesc( Sacmem, &cmvars.deleteflag, &cmvars.readonlyflag, 
	 &cmvars.indirectflag, &cmvars.sharedflag, &cmvars.reservedflag, 
	 &cmvars.applflag1, &cmvars.applflag2, &cmvars.valuetype, &cmvars.desclength, 
	 &cmvars.namelength, &cmvars.valuelength );
	if( cmvars.valuetype == VALUENIL ){
		*nilindex = cmvars.descindex;
                Sacmem += cmvars.desclength;
		zgetc( Sacmem, kname , 4 );
		}
	else{
		cmvars.descindex = cmvars.descindex + cmvars.desclength + 
		 cmvars.namelength + cmvars.valuelength;
                Sacmem += cmvars.desclength + cmvars.namelength + cmvars.valuelength;
		goto L_1000;
		}

L_8888:
	return;

} /* end of function */

