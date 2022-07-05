#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"

int /*FUNCTION*/ lbsrch(ksrch, ncsrch, klist, klist_s, nlist, 
	 index)
char *ksrch;
int ncsrch;
char *klist;   int klist_s;
int nlist, *index;
{
#define KLIST(I_,J_)	(klist+(I_)*(klist_s)+(J_))
	int lbsrch_v;
	int jbot, jcur, jtop;

	/*=====================================================================
	 * PURPOSE: To perform alphanumeric binary search.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KSRCH:   Character string to search for. [c]
	 *    NCSRCH:  Number of characters in KSRCH. [i]
	 *    KLIST:   List to search [c]
	 *    NLIST:   Length of KLIST. [i]
	 *=====================================================================
	 * FUNCTION VALUE: Set to .TRUE. if KSRCH was found in KLIST.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    INDEX:   Index in KLIST of match. Set to 0 if no match. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    JCUR:    JLIST index pointing to the item currently being checked.
	 *    JBOT:    JLIST index pointing to the bottom of the possible matches.
	 *    JTOP:    JLIST index pointing to the top of the possible matches.
	 *=====================================================================
	 * ASSUMPTIONS:
	 * - KLIST is in lexically sorted (in alphanumeric order.)
	 * - FORTRAN 77 functions LLT and LGT exist.
	 *=====================================================================
	 * LIMITATIONS:
	 * - Not searching for multiple matches.
	 *   Returning after first match is found.
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Assume the worst. */
	*index = 0;
	lbsrch_v = FALSE;

	/* - Return immediately if string being search for is not
	 *   lexically contained within list. */

	if( llt2( ksrch, KLIST(0,0), ncsrch ) || 
	 lgt2( ksrch, KLIST(nlist - 1,0), ncsrch) ){
		goto L_8888;
		}
	else{
		jbot = 0;
		jtop = nlist + 1;
		}

	/* - Halve the search interval each iteration until a match is found or
	 *   the list is exhausted. */

L_1000:
	jcur = (jtop + jbot)/2;
	if( memcmp(ksrch,KLIST(jcur - 1,0),ncsrch) == 0 ){
		*index = jcur;
		lbsrch_v = TRUE;
		}
	else if( lgt2( ksrch, KLIST(jcur - 1,0), ncsrch) ){
		jbot = jcur;
		if( (jtop - jbot) != 1 )
			goto L_1000;
		}
	else{
		jtop = jcur;
		if( (jtop - jbot) != 1 )
			goto L_1000;
		}

L_8888:
	return( lbsrch_v );

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820617:  Original version.
	 *===================================================================== */

#undef	KLIST
} /* end of function */

