#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ setcolorname(color, color_s)
char *color;   int color_s;
{
	char ktest[9];
	int index, nc;



	/*=====================================================================
	 * PURPOSE: To set the color attribute by name.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    color:   The name of the desired color. [c]
	 *             ='WHITE', 'RED', 'GREEN', 'BLUE',
	 *              'YELLOW', 'CYAN', 'MAGENTA', or 'BLACK'
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *     mach:
	 *     gdm:    ctname, nctsize
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - Association of color names with color table index numbers
	 *   is coupled with the color table set by readctable.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  upcase, lequal, setcolor
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861020:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861020
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of input color name and convert it to upper case. */
	nc = min( indexb( color,color_s ), MCPW );
	upcase( color, nc, ktest,9 );

	/* - Test name versus list of names in default color table.
	 *   Set requested color if found. */

	if( lequal( ktest,9, (char*)kmgdm.ctname[1],9, cmgdm.nctsize, 
	 &index ) ){
		setcolor( index );

		/* - If not found, set to foreground color. */

		}
	else{
		setcolor( cmgdm.nctsize );

		}

L_8888:
	return;

} /* end of function */

