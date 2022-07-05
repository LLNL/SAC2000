#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ xgrid(nerr)
int *nerr;
{



	/*=====================================================================
	 * PURPOSE: To parse the parameter-setting command GRID.
	 *          This command controls grid-plotting attributes.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     ISOLID, IDOT
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     LXGRD, LYGRD, IXGRD, IYGRD
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- Turn gridding on or off. */
		if( lclog( &cmgem.lxgrd ) ){
			cmgem.lygrd = cmgem.lxgrd;

			/* -- Change to solid linestyle. */
			}
		else if( lckey( "S$",3 ) ){
			cmgem.ixgrd = cmgem.isolid;
			cmgem.iygrd = cmgem.isolid;
			cmgem.lxgrd = TRUE;
			cmgem.lygrd = TRUE;

			/* -- Change to dotted linestyle. */
			}
		else if( lckey( "D$",3 ) ){
			cmgem.ixgrd = cmgem.idot;
			cmgem.iygrd = cmgem.idot;
			cmgem.lxgrd = TRUE;
			cmgem.lygrd = TRUE;

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;
		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    850321:  Typing SOLID or DOTTED now turns on gridding.
	 *    820611:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850321
	 *===================================================================== */

} /* end of function */

