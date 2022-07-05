#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/cnd.h"
void /*FUNCTION*/ xenddo(nerr)
int *nerr;
{
	int i, i_;
        FILE *nun;


	/*=====================================================================
	 * PURPOSE: To parse the action command ENDDO.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: .
	 *=====================================================================
	 * MODULE/LEVEL:  CND/2
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    ndolevel: Decremented by one.
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
	if( cnd.ndolevel > 0 ){
		getclun( &nun, nerr );

		/* -- End of WHILE, backup to WHILE statement */

		if( Ndotype[cnd.ndolevel] == 1 ){

                        backspace(nun,Ndolines[cnd.ndolevel] + 1);
			cnd.ndolevel = cnd.ndolevel - 1;

			/* -- End of DO, get next loop variable */

			}
		else{
			if( ldolist( nerr ) ){

                                backspace(nun, Ndolines[cnd.ndolevel]);
				/*    No more variables in this do list */

				}
			else{
				deletev( (char*)kcnd.kdovar[cnd.ndolevel - 1],MCPFN+1, 
				 (char*)kcnd.kdolist[cnd.ndolevel - 1],MCPFN+1, nerr );
				cnd.ndolevel = cnd.ndolevel - 1;
				}
			}

		/* - Raise error condition if not in an do condition. */

		}
	else{
		*nerr = 1;

		}

L_8888:
	return;

} /* end of function */

