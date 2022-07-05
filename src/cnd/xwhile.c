#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/cnd.h"
#include "../../inc/cpf.h"
void /*FUNCTION*/ xwhile(nerr)
int *nerr;
{
	char kcond[MCMSG+1], kmacroname[MCPFN+1], kresult[9];
	int nc, nchar, numlines, nverr;



	/*=====================================================================
	 * PURPOSE: To parse the action command WHILE.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: .
	 *=====================================================================
	 * MODULE/LEVEL:  CND/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG, MCPFN
	 *    cpf:     kvarsname
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    ndolevel: Incremented by one.
	 *    lifresp:  Set to value of if test.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970128:  Fixed a bug in nesting while loops.  maf
	 *    870817:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870415
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	numlines = 0;

	/* -- Get length of do loop */

	getdolen( &numlines, nerr );
	if( *nerr != 0 ){
		apcmsg( "in macro file",14 );
		getvvstring( kmcpf.kvarsname,9, "macroname",10, &nc, kmacroname
		 ,MCPFN+1, &nverr );
		apcmsg( kmacroname,MCPFN+1 );
		goto L_8888;
		}

	/* - Copy rest of command to condition string. */

        memset(kcond,(int)' ',MCMSG);
        kcond[MCMSG] = '\0';

	lcrest( MCMSG, kcond,MCMSG+1, &nchar );

	/* - Evaluate condition */

	evallogical( kcond,MCMSG+1, kresult,9 );
	if( memcmp(kresult,"ERROR",5) == 0 )
		goto L_9000;

	if( memcmp(kresult,"TRUE",4) == 0 ){
		cnd.ndolevel = cnd.ndolevel + 1;
		Ndotype[cnd.ndolevel] = 1;
		Ndolines[cnd.ndolevel] = numlines;
		}
	else if( memcmp(kresult,"FALSE",5) == 0 ){
		cnd.ndolevel++ ;	/* added 970128 to fix a bug in nesting. */
		skipdo( nerr );
		}
	else{
		*nerr = 1;
		}

L_8888:
	return;

L_9000:
	*nerr = 2703;
	setmsg( "ERROR", *nerr );
	apcmsg( kcond,MCMSG+1 );
	goto L_8888;

} /* end of function */

