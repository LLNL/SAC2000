#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/cnd.h"
void /*FUNCTION*/ xelseif(nerr)
int *nerr;
{
	char kcond[MCMSG+1], kresult[9];
	int nchar;



	/*=====================================================================
	 * PURPOSE: To parse the action command ELSEIF.
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
	 *    870817:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870415
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Copy rest of command into the condition string and evaluate it. */

	if( cnd.niflevel > 0 ){
		if( Lifresp[cnd.niflevel] ){
			skipif( nerr );
			}
		else{
                        memset(kcond,(int)' ',MCMSG);
                        kcond[MCMSG] = '\0';
			lcrest( MCMSG, kcond,MCMSG+1, &nchar );
			evallogical( kcond,MCMSG+1, kresult,9 );
			if( memcmp(kresult,"ERROR",5) == 0 )
				goto L_9000;
			if( memcmp(kresult,"TRUE",4) == 0 )
				Lifresp[cnd.niflevel] = TRUE;
			if( memcmp(kresult,"FALSE",5) == 0 )
				Lifresp[cnd.niflevel] = FALSE;
			if( !Lifresp[cnd.niflevel] ){
				skipif( nerr );
				}
			}
		}
	else{
		*nerr = 1;
		}

L_8888:
	return;

L_9000:
	*nerr = 1;
	goto L_8888;

} /* end of function */

