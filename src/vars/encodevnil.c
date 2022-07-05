#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ encodevnil(node, offset, nerr)
int node, offset, *nerr;
{
	int _l0;
	void encodevdesc(), zputc();
        int *Isacmem;
        float *Sacmem;

	/*=====================================================================
	 * PURPOSE:  To encode an entry of type "NIL" at end of vars section.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    node:    The node number of the vars list. [i]
	 *    offset:  The offset of the nil descriptor into the list. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    MAXCVNAME, VALUENIL, varsindex, varslength, BIGVALUE
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    descindex, valuetype, namelength, valuelength,
	 *             desclength, deleteflag, readonlyflag, indirectflag,
	 *             sharedflag, reservedflag, applflag1, applflag2,
	 *             varsnilindex
	 *    mem:     isacmem
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  encodevdesc, zputc
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881205:  Original version extracted from createvlist.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881205
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Determine descriptor index and value length of NIL entry.
	 *   (Two words are subtracted from length to account for nil descriptor and name.) */

	cmvars.descindex = Varsindex[node] + offset;
	cmvars.valuelength = Varslength[node] - offset - 2;
	cmvars.desclength = 1;
	if( cmvars.valuelength > BIGVALUE ){
		cmvars.desclength = cmvars.desclength + 1;
		cmvars.valuelength = cmvars.valuelength - 1;
		}

	/* - Prepare rest of descriptor. */

	cmvars.valuetype = VALUENIL;
	fstrncpy( kmvars.valuename, MAXCVNAME, "NIL", 3 );
	cmvars.namelength = 1;
	cmvars.deleteflag = FALSE;
	cmvars.readonlyflag = FALSE;
	cmvars.indirectflag = FALSE;
	cmvars.sharedflag = FALSE;
	cmvars.reservedflag = FALSE;
	cmvars.applflag1 = FALSE;
	cmvars.applflag2 = FALSE;

#ifdef DEBUG
  malloc_verify();
#endif
	/* - Encode descriptor. */
        Isacmem = (int*)cmmem.sacmem[Varsindex[node]]+offset;
	encodevdesc( Isacmem, &cmvars.deleteflag, &cmvars.readonlyflag, 
	 &cmvars.indirectflag, &cmvars.sharedflag, &cmvars.reservedflag, 
	 &cmvars.applflag1, &cmvars.applflag2, &cmvars.valuetype, &cmvars.desclength, 
	 &cmvars.namelength, &cmvars.valuelength );
#ifdef DEBUG
  malloc_verify();
#endif

	/* - Encode name. */
        Sacmem = cmmem.sacmem[Varsindex[node]]+offset+cmvars.desclength;
	zputc( kmvars.valuename,MAXCVNAME+1, Sacmem, 4*cmvars.namelength );

#ifdef DEBUG
  malloc_verify();
#endif

	/* - Store index of new nil descriptor. */

	Varsnilindex[node] = cmvars.descindex;

L_8888:
	return;

} /* end of function */

