#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "complex.h"
#include "proto.h"
#include "mach.h"
int /*FUNCTION*/ nequal(ksrch, klist, klist_s, nlist)
char *ksrch, *klist;   int klist_s;
int nlist;
{
#define KLIST(I_,J_)	(klist+(I_)*(klist_s)+(J_))
	int jdx, jdx_, nequal_v;

	/* FUNCTION TO SEARCH A LIST OF CHARACTER VARIABLES FOR A CERTAIN
	 * CHARACTER STRING AND TO RETURN THE INDEX OF THE MATCH IF FOUND. */
	/*     KSRCH:     CHARACTER VARIABLE TO SEARCH FOR.
	 *     KLIST:     LIST OF CHARACTER VARIABLES TO SEARCH.
	 *     NLIST:     NUMBER OF VARIABLES IN KLIST.
	 *      NERR:     ERROR FLAG. SET TO 0 IF NO ERROR OCCURRED.
	 *    NEQUAL:=    INDEX OF THE MATCH IF FOUND. ZERO OTHERWISE. */

	/* PROCEDURE: */

	nequal_v = 0;

	for ( jdx = 1 ; jdx <= nlist ; jdx++ ) {
		jdx_ = jdx - 1 ;
		if( memcmp ( ksrch , KLIST ( jdx_ , 0 ) ,
		    min ( strlen ( ksrch ) , klist_s ) ) == 0 ) {
			nequal_v = jdx ;
			return( nequal_v ) ;
		}
	}

	return( nequal_v ) ;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    810000:  Original version.
	 *===================================================================== */

#undef	KLIST
} /* end of function */

