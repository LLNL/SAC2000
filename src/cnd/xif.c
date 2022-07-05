#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/cnd.h"
void /*FUNCTION*/ xif(nerr)
int *nerr;
{
	char kcond[MCMSG+1], kresult[9], ktoken[17];
	int nchar;



	/*=====================================================================
	 * PURPOSE: To parse the action command IF.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: .
	 *=====================================================================
	 * MODULE/LEVEL:  CND/2
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    niflevel: Incremented by one.
	 *    lifresp:  Set to value of if test.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871026:  Fixed bug encoding floating point numbers into "kcond".
	 *             Added error number and message.
	 *    870817:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870415
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Get rest of command line and store in condition string. */

        memset(kcond,(int)' ',MCMSG);
        kcond[MCMSG] = '\0';

	lcrest( MCMSG, kcond,MCMSG+1, &nchar );

	/* - Evaluate condition. */

	evallogical( kcond,MCMSG+1, kresult,9 );
	if( memcmp(kresult,"ERROR",5) == 0 )
		goto L_9000;
	cnd.niflevel = cnd.niflevel + 1;
	if( memcmp(kresult,"TRUE",4) == 0 )
		Lifresp[cnd.niflevel] = TRUE;
	if( memcmp(kresult,"FALSE",5) == 0 )
		Lifresp[cnd.niflevel] = FALSE;
	if( !Lifresp[cnd.niflevel] )
		skipif( nerr );

L_8888:
	return;

L_9000:
	*nerr = 2703;
	setmsg( "ERROR", *nerr );
	apcmsg( kcond,MCMSG+1 );
	goto L_8888;

} /* end of function */

