#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
int /*FUNCTION*/ nccomp(ktoken, klist, klist_s, nlist, nchar)
char *ktoken, *klist;   int klist_s;
int nlist, nchar;
{
#define KLIST(I_,J_)	(klist+(I_)*(klist_s)+(J_))
	int j, j_, nccomp_v;

	/*=====================================================================
	 * PURPOSE: To compare a token against a list of other tokens.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KTOKEN:  Token to search for.
	 *    KLIST:   List of tokens to search against.
	 *    NLIST:   Length of KLIST.
	 *    NCHAR:   Character length of KTOKEN and KLIST.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    NCCOMP:  = 0 if KTOKEN was not found in KLIST.
	 *             = index into KLIST of match if found.
	 *=====================================================================
	 * MODULE/LEVEL:
	 *===================================================================== */
	/* PROCEDURE: */
	nccomp_v = 0;
	for( j = 1; j <= nlist; j++ ){
		j_ = j - 1;
		if( memcmp(ktoken,KLIST(j_,0),nchar) == 0 ){
			nccomp_v = j;
			goto L_8888;
			}
		}

L_8888:
	return( nccomp_v );

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820419:  Original version.
	 *===================================================================== */

#undef	KLIST
} /* end of function */

