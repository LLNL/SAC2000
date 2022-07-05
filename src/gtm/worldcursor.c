#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gtm.h"
void /*FUNCTION*/ worldcursor(xwloc, ywloc, kchar)
float *xwloc, *ywloc;
byte *kchar;
{
	float xloc, yloc;



	/*=====================================================================
	 * PURPOSE:  To perform "locator" graphics input function.
	 *           The cursor is turned on and initially placed at the
	 *           world coordinate location given by xwloc and ywloc.
	 *           When a single character is typed at the terminal,
	 *           the new cursor location and character are returned.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    xwloc:   Initial X world coordinate for cursor. [f]
	 *    ywloc:   Initial Y world coordinate for cursor. [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    xwloc:   Current X world coordinate of cursor. [f]
	 *    ywloc:   Current Y world coordinate of cursor. [f]
	 *    kchar:   Alphanumeric character typed at terminal. [c1]
	 *=====================================================================
	 * MODULE/LEVEL:  gtm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gtm:     xmpwv1, xmpwv2, ympwv1, ympwv2
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  worldmove, cursor
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861027:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861027
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Move to input world coordinate position. */
	worldmove( *xwloc, *ywloc );

	/* - Get character and cursor position (in viewport coordinates.) */

	cursor0( &xloc, &yloc, kchar );

	/* - Convert viewport location to world coordinates. */

	*xwloc = (xloc - cmgtm.xmpwv2)/cmgtm.xmpwv1;
	*ywloc = (yloc - cmgtm.ympwv2)/cmgtm.ympwv1;

L_8888:
	return;

} /* end of function */

