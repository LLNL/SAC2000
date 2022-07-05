#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ xaxes(nerr)
int *nerr;
{
	int ltf;
	int index;



	/*=====================================================================
	 * PURPOSE: To parse the parameter-setting command AXES.
	 *          This command sets axes drawing attributes.
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
	 *    GEM:     LTOPAX, LBOTAX, LRIGAX, LLEFAX
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
			cmgem.ltopax = FALSE;
			cmgem.lbotax = FALSE;
			cmgem.lrigax = FALSE;
			cmgem.llefax = FALSE;
			ltf = TRUE;

			/* -- Set flag to either turn axes on or off. */
			}
		else if( lclog( &ltf ) ){

			/* --- A particular axes name to turn on or off. */
			}
		else if( lclist( (char*)kmgem.ksides,9, 4, &index ) ){
			if( index == cmgem.ibot ){
				cmgem.lbotax = ltf;
				}
			else if( index == cmgem.ileft ){
				cmgem.llefax = ltf;
				}
			else if( index == cmgem.iright ){
				cmgem.lrigax = ltf;
				}
			else if( index == cmgem.itop ){
				cmgem.ltopax = ltf;
				}

			/* --- Change status of all axes. */
			}
		else if( lckey( "A$",3 ) ){
			cmgem.lbotax = ltf;
			cmgem.llefax = ltf;
			cmgem.lrigax = ltf;
			cmgem.ltopax = ltf;

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

