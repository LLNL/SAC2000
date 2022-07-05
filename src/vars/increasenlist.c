#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ increasenlist(node, newlength, nerr)
int node, newlength, *nerr;
{
	int existsvlist;
	int icchild1, icchild2, ichild2, icparent2, ioffset, newindex, 
	 nilindex, nillength, niloffset, oldindex, oldlength, oldnilindex, 
	 oldvaluelength, type;
	void decodevdesc(), encodevdesc();



	/*=====================================================================
	 * PURPOSE:  To increase the size of a vars list in memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    node:       Node number of vars list. [i]
	 *    newlength:  New length of vars list. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    varsindex, varslength, VARSLISTNOTFOUND, MAXCVNAME
	 *             VALUELIST, BIGVALUE
	 *    mem:     sacmem
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    varsindex, varslength, valuename, 
	 *             deleteflag, readonlyflag, indirectflag, 
	 *             desclength, namelength, valuelength, varsnilindex
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  allamb, copy, fill, encodevdesc, splitvname,
	 *             relamb, setmsg, apcmsg, encodevnil
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    901012:  Fixed bug in encoding a new nil terminator for a valuelength
	 *             greater than 65536. (BKH)
	 *    890327:  Changed to reference by node number rather than vars name.
	 *             Fixed problem of new descriptor being two words int.
	 *    890309:  Fixed bug in redefining vars list descriptor.
	 *    880406:  Was not setting vars list type correctly.
	 *    880322:  Was not computing the new vars nil terminator index.
	 *    871019:  Was not updating internal vars list length.
	 *    871013:  Implemented allocation logic.
	 *    861229:  Original stub version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  871013
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Allocate a new memory block. */

	allamb( &cmmem, newlength, &newindex, nerr );
	if( *nerr != 0 )
		goto L_8888;

        /*   Zero out the newly allocated memory */
        memset(cmmem.sacmem[newindex],'\0',newlength*sizeof(float));

	/* - Retrieve old index and length.  Compute offset to nil descriptor. */

	oldindex = Varsindex[node];
	oldlength = Varslength[node];
	niloffset = Varsnilindex[node] - Varsindex[node];

	/* - Update index and length values. */

	Varsindex[node] = newindex;
	Varslength[node] = newlength;

	/* - Determine name of child vars list. */

	ichild2 = Ncvarsname[node];
	splitvname( (char*)kmvars.varsname[node - 1],MAXCVNAME+1, icchild2, &icparent2, 
	 &icchild1 );
	fstrncpy( kmvars.valuename, MAXCVNAME, kmvars.varsname[node - 1]+icchild1 - 
	 1,min(icchild2,MAXCVNAME) - icchild1 + 1);

	/* - Decode old vars list descriptor. */

	decodevdesc( cmmem.sacmem[oldindex], &cmvars.deleteflag, &cmvars.readonlyflag, 
	 &cmvars.indirectflag, &cmvars.sharedflag, &cmvars.reservedflag, 
	 &cmvars.applflag1, &cmvars.applflag2, &cmvars.valuetype, &cmvars.desclength, 
	 &cmvars.namelength, &cmvars.valuelength );


	/* - In the (extremely rare) case where the new valuelength is greater than
	 *   65536 and the old value length lesser than 65536 we have to account
	 *   for the fact that the new descriptor will use two words while the
	 *   old descriptor used only one word. */

	oldvaluelength = cmvars.valuelength;
	cmvars.valuelength = newlength - cmvars.desclength - cmvars.namelength;
	if( cmvars.valuelength > BIGVALUE && oldvaluelength <= BIGVALUE ){
		cmvars.desclength = 2;
		cmvars.valuelength = cmvars.valuelength - 1;
		ioffset = 1;
		}
	else{
		ioffset = 0;
		}

	/* - Copy old data to new block and zero fill to new length. */

	copy( (int*)cmmem.sacmem[oldindex], (int*)cmmem.sacmem[newindex] + ioffset, 
	 oldlength );
	fill( cmmem.sacmem[newindex]+ioffset+oldlength, newlength - ioffset - 
	 oldlength, 0. );

	/* - Encode vars list descriptor with new length of entire list. */

	encodevdesc( cmmem.sacmem[newindex], &cmvars.deleteflag, &cmvars.readonlyflag, 
	 &cmvars.indirectflag, &cmvars.sharedflag, &cmvars.reservedflag, 
	 &cmvars.applflag1, &cmvars.applflag2, &cmvars.valuetype, &cmvars.desclength, 
	 &cmvars.namelength, &cmvars.valuelength );

	/* - Encode a new nil terminator.
	 *   modified by BKH: 10/11/90  old: call encodevnil(node,niloffset,nerr) */
	encodevnil( node, niloffset + ioffset, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Release old memory block. */

	relamb( cmmem.sacmem, oldindex, nerr );
	if( *nerr != 0 )
		goto L_8888;

L_8888:
	return;

} /* end of function */

