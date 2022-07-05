#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ xticks(nerr)
int *nerr;
{
	int ltf;
	int index;



	/*=====================================================================
	 * PURPOSE: To parse the parameter-setting command TICKS.
	 *          This command sets tick mark drawing attributes.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     KSIDES
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     LTOPTC, LBOTTC, LRIGTC, LLEFTC
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCLOG, LCKEY, LCLIST
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Assume user wants to turn axes on. */

	ltf = TRUE;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- Set flag so that only listed axes are turned on. */
		if( lckey( "ONL$",5 ) ){
			cmgem.ltoptc = FALSE;
			cmgem.lbottc = FALSE;
			cmgem.lrigtc = FALSE;
			cmgem.lleftc = FALSE;
			ltf = TRUE;

			/* -- Set flag to either turn axes on or off. */
			}
		else if( lclog( &ltf ) ){

			/* --- A particular axes name to turn on or off. */
			}
		else if( lclist( (char*)kmgem.ksides,9, 4, &index ) ){
			if( index == cmgem.ibot ){
				cmgem.lbottc = ltf;
				}
			else if( index == cmgem.ileft ){
				cmgem.lleftc = ltf;
				}
			else if( index == cmgem.iright ){
				cmgem.lrigtc = ltf;
				}
			else if( index == cmgem.itop ){
				cmgem.ltoptc = ltf;
				}

			/* --- Change status of all axes. */
			}
		else if( lckey( "A$",3 ) ){
			cmgem.lbottc = ltf;
			cmgem.lleftc = ltf;
			cmgem.lrigtc = ltf;
			cmgem.ltoptc = ltf;

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
	 *    820610:  Original version.
	 *===================================================================== */

} /* end of function */

