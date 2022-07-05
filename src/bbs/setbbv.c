#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/bbs.h"
void /*FUNCTION*/ setbbv(kname, kvalue, nerr, kname_s, kvalue_s)
char *kname;   int kname_s;
char *kvalue;   int kvalue_s;
int *nerr;
{
	char ktemp[33] = "                                " ;
	int nc;

        kname_s ++ ;    /* Make code compatible with C and FORTRAN. */
        kvalue_s++ ;    /* maf 970929 */

	/*=====================================================================
	 * PURPOSE:  To set (define) a blackboard variable.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kname:   The name of the blackboard variable. [c]
	 *    kvalue:  The value of the blackboard variable. [c]
	 *    nerr:    Error return flag.  Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL: bbs/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    
	 *    bbs:     knmbbs
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  indexb, modcase, putvvstring
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870301:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870301
	 *===================================================================== */
	/* PROCEDURE: */
	nc = min( indexb( kname,kname_s ), 32 );
	modcase( TRUE, kname, nc, ktemp );
	nc = indexb( kvalue,kvalue_s );
	putvvstring( kmbbs.knmbbs,MCPFN+1, ktemp,33, nc, kvalue,kvalue_s, 
	 nerr );

L_8888:
	return;

} /* end of function */





/* Added for FORTRAN friendliness */
void setbbv_ (kname, kvalue, nerr, kname_s, kvalue_s)
char *kname;   int kname_s;
char *kvalue;   int kvalue_s;
int *nerr;
{
	setbbv(kname, kvalue, nerr, kname_s, kvalue_s) ;
}
