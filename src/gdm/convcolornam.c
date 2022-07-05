#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ convcolorname(name, number)
char *name;
int *number;
{
	char ktest[9];



	/*=====================================================================
	 * PURPOSE: To convert a color name to it's equivalent color number.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *     name:   The name of the desired color. [c]
	 *             ='WHITE', 'RED', 'GREEN', 'BLUE', etc.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *   number:   The equivalent color number. [i]
	 *             A value of 0 is returned if the name was not found.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     ctname, nctsize
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - Association of color names with color table index numbers
	 *   coupled with the color table defined by setctable.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  upcase, lequal
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871216:  Subtracted 1 to number -- was wrong because of previous
	 *             modification.
	 *    871001:  Changed so it would also check the first entry in the
	 *             color table.
	 *    861020:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861020
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Convert input color name to upper case. */
	upcase( name, MCPW, ktest,9 );

	/* - Test name versus list of names in default color table. */

	if( lequal( ktest,9, (char*)kmgdm.ctname[0],9, cmgdm.nctsize, 
	 number ) ){
		*number = *number - 1;

		/* - If not found, return a zero. */

		}
	else{
		*number = cmgdm.nctsize;

		}

L_8888:
	return;

} /* end of function */

