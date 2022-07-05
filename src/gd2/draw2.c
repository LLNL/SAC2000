#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gd2.h"
void /*FUNCTION*/ draw2(xloc, yloc)
double xloc, yloc;
{
	int ixloc, iyloc, nerr;



	/*=====================================================================
	 * PURPOSE:  To perform DRAW operation on graphics device 2 (SGF).
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    XLOC:    X viewport location. [f]
	 *    YLOC:    Y viewport location. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  GD2/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GD2:     JFBMAX
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GD2:     MFBUF, JFBPNT
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861016:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  861016
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Scale floating point values to the devices coordinates. */
	ixloc = xloc*XW;
	iyloc = yloc*XW;

	/* - Store location in buffer. */

	Mfbuf[cmgd2.jfbpnt] = ixloc;
	Mfbuf[cmgd2.jfbpnt + 1] = iyloc;
	cmgd2.jfbpnt = cmgd2.jfbpnt + 2;

	/* - Flush buffer if necessary. */

	if( cmgd2.jfbpnt > JFBMAX )
		flushbuffer2( &nerr );

L_8888:
	return;

} /* end of function */

