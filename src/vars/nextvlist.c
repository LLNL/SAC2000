#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
int /*FUNCTION*/ nextvlist(index, name, name_s)
int *index;
char *name;   int name_s;
{
	int nextvlist_v;
	int _l0;
	void decodevdesc(), zgetc();

        float *Sacmem;

	/*=====================================================================
	 * PURPOSE:  To get the next vars sublist entry in a vars list.
	 *           Used for sequential access of a vars list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    index:   Index of next vars entry in vars list. [i]
	 *             This value is initially obtained from a call to "initvlist".
	 *             It is updated on each call to this function.
	 *             DO NOT CHANGE THIS VALUE BETWEEN CALLS TO THIS FUNCTION.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    name:    Name of vars sublist entry. [c]
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    nextvlist:  Set to .TRUE. if the entry was valid.
	 *                Set to .FALSE. if end of vars list found
	 *                or an error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    VALUENIL, VALUELIST
	 *    mem:     sacmem
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    desclength, namelength, valuelength
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  decodevdesc, zgetc
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890309:  Added output of node number.
	 *    881114:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881114
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Using the input vars entry pointer, decode the vars descripter,
	 *   saving descriptor information in vars common block variables. */
L_1000:
        Sacmem = cmmem.sacmem[cmvars.varsnode1] + (*index - cmvars.varsnode1);
	decodevdesc( Sacmem, &cmvars.deleteflag, &cmvars.readonlyflag, 
	 &cmvars.indirectflag, &cmvars.sharedflag, &cmvars.reservedflag, 
	 &cmvars.applflag1, &cmvars.applflag2, &cmvars.valuetype, &cmvars.desclength, 
	 &cmvars.namelength, &cmvars.valuelength );

	/* - If type is a NIL, signifying the end of the vars list, set function
	 *   value to FALSE, and describe condition in output arguments. */

	if( cmvars.valuetype == VALUENIL ){
		nextvlist_v = FALSE;
		fstrncpy( name, name_s-1, "ENDLIST", 7 );

		/* - If type is LIST, return vars entry name and increment vars entry pointer. */

		}
	else if( cmvars.valuetype == VALUELIST ){
		nextvlist_v = TRUE;
		fstrncpy( name, name_s-1, " ", 1 );
                Sacmem = cmmem.sacmem[cmvars.varsnode1] + (*index - cmvars.varsnode1)
                                                        + cmvars.desclength;
		zgetc( Sacmem, name, 4*cmvars.namelength );
		*index = *index + cmvars.desclength + cmvars.namelength + 
		 cmvars.valuelength;

		/* - If any other type, increment vars entry pointer and loop. */

		}
	else{
		*index = *index + cmvars.desclength + cmvars.namelength + 
		 cmvars.valuelength;
		goto L_1000;
		}

L_8888:
	return( nextvlist_v );

} /* end of function */

