#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ convcolornum(number, name, name_s)
int number;
char *name;   int name_s;
{
	char ktest[9];



	/*=====================================================================
	 * PURPOSE: To convert a color number to it's equivalent color name.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *   number:   The number of the desired color. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *     name:   The equivalent name of the desired color. [c]
	 *             ='WHITE', 'RED', 'GREEN', 'BLUE', etc.
	 *             Set to 'UNKNOWN' if a bad number was input.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     ctname, nctsize
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - Association of color names with color table index numbers
	 *   is coupled with the default color table set by SETCTABLE.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861020:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861020
	 *===================================================================== */
	/* PROCEDURE: */
	/* - If number is in the correct range. */
	if( number > 0 && number <= cmgdm.nctsize ){
		fstrncpy( name, name_s-1, kmgdm.ctname[number], strlen(kmgdm.ctname[number]));

		/* - If not found, return 'UNKNOWN'. */

		}
	else{
		fstrncpy( name, name_s-1, "UNKNOWN", 7);

		}

L_8888:
	return;

} /* end of function */

