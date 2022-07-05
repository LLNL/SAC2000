#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ getvvstring(vars, vars_s, name, name_s, numchars, 
	 value, value_s, nerr)
char *vars;   int vars_s;
char *name;   int name_s;
int *numchars;
char *value;   int value_s;
int *nerr;
{
	int index, ncget, ncvalue, numwords, type;
	void zgetc();

        float *Sacmem;

	/*=====================================================================
	 * PURPOSE:  To get a vars value of type STRING.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:       Name of vars list. [c]
	 *    name:       Name of vars entry. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    numchars:   Number of characters in output string. [i]
	 *    value:      Output string [c]
	 *    nerr:       Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    VALUESTRING
	 *    mem:     sacmem
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getventry, zgetc, indexb
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881107:  Changed name of subroutine.
	 *    870302:  Blank filled value field before returning.
	 *    861229:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881107
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	type = VALUESTRING;

	/* - Get requested entry. */

	getventry( vars,vars_s, name,name_s, type, &numwords, &index, 
	 nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Move characters to value field. */

	ncvalue = (value_s - 1);
	ncget = min( ncvalue, 4*numwords );

/* making the assumption here that cmvars.varsnode points to the node of interest */
        Sacmem = cmmem.sacmem[Varsindex[cmvars.varsnode]]+(index-Varsindex[cmvars.varsnode]);
	zgetc( Sacmem, value, ncget );

	/* - Pad value field with blanks if necessary. */

	if( ncget < ncvalue )
		subscpy( value, ncget, -1, value_s - 1, " " );

	/* - Determine the length of the value field without trailing blanks. */

	*numchars = indexb( value,value_s );

L_8888:
	return;

} /* end of function */

