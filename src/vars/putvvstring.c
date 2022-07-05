#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ putvvstring(vars, vars_s, name, name_s, numchars, 
	 value, value_s, nerr)
char *vars;   int vars_s;
char *name;   int name_s;
int numchars;
char *value;   int value_s;
int *nerr;
{
	char lastword[5] = "    " ;
	int _l0, index, length, numwhole, numwords, type;
	void zputc();

        float *Sacmem;

	/*=====================================================================
	 * PURPOSE:  To put a vars value of type STRING.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:       Name of vars list. [c]
	 *    name:       Name of vars entry. [c]
	 *    numchars:   Number of characters in value. [i]
	 *                If <= 0, then this value is computed to be the
	 *                length of the string without trailing blanks. 
	 *    value:      Input string. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:       Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    VALUESTRING
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    mem:     sacmem
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  putventry
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881107:  Changed name of subroutine.
	 *    861229:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881107
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

#ifdef DEBUG
  malloc_verify();
#endif
	type = VALUESTRING;

	if( numchars > 0 ){
		length = numchars;
		}
	else{
		length = indexb( value,value_s );
		}
	numwords = (length - 1)/4 + 1;

#ifdef DEBUG
  malloc_verify();
#endif

	putventry( vars,vars_s, name,name_s, &type, numwords, &index, 
	 nerr );
	if( *nerr != 0 )
		goto L_8888;
#ifdef DEBUG
  malloc_verify();
#endif

        Sacmem = cmmem.sacmem[Varsindex[cmvars.varsnode]]+(index-Varsindex[cmvars.varsnode]);
	if( 4*numwords == length ){
		zputc( value,value_s, Sacmem, 4*numwords );
		}
	else{
		numwhole = numwords - 1;
		if( numwhole >= 1 )
			zputc( value,value_s, Sacmem, 4*numwhole );
		fstrncpy( lastword, 4, value+numwhole*4,length - (4*numwhole + 
		 1) + 1);
		zputc( lastword,5, Sacmem+numwords-1, 4 );
		}

#ifdef DEBUG
  malloc_verify();
#endif

L_8888:
	return;

} /* end of function */

