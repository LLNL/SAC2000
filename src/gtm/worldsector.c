#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ worldsector(xwcen, ywcen, radius, deg1, deg2, degi)
double xwcen, ywcen, radius, deg1, deg2, degi;
{
	int _d_l, _d_m, _do0, _do1;
	float rdeg, rdeg1, rdeg2, rdeg_, rdegi, x, y;

	/*=====================================================================
	 * PURPOSE:  To plot a sector of a circle in world coordinate system.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    xwcen:   X world coordinate of center of circle. [f]
	 *    ywcen:   Y world coordinate of center of circle. [f]
	 *    radius:  Radius of circle in world units. [f]
	 *    deg1:    Starting angle for sector in degrees clockwise
	 *             from vertical. [f]
	 *    deg2:    Stopping angle for sector. [f]
	 *    degi:    Increment angle in degrees. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  gtm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    torad
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  worldmove, worlddraw
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    830925:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861027
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Convert input angles which are in degrees to radians. */
	rdeg1 = TORAD*deg1;
	rdeg2 = TORAD*deg2;
	rdegi = TORAD*degi;

	/* - Move to first data point. */

	x = xwcen + radius*sin( rdeg1 );
	y = ywcen + radius*cos( rdeg1 );
	worldmove( x, y );

	/* - Draw to rest of points on sector, using given increment. */

        for( rdeg = rdeg1+rdegi; rdeg <= rdeg2; rdeg += rdegi) {
		x = xwcen + radius*sin( rdeg );
		y = ywcen + radius*cos( rdeg );
		worlddraw( x, y );
		}

	/* - Draw to last point. */

	x = xwcen + radius*sin( rdeg2 );
	y = ywcen + radius*cos( rdeg2 );
	worlddraw( x, y );

L_8888:
	return;

} /* end of function */

