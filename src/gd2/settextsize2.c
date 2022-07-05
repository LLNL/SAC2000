#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gd2.h"
void /*FUNCTION*/ settextsize2(width, height)
double width, height;
{
	int nerr;



	/*=====================================================================
	 * PURPOSE:  To change size of graphics text for device 2 (SGF).
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    WIDTH:   The width of a single character in viewport units. [f]
	 *    HEIGHT:  The height of a single line in viewport units. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  GD2/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GD2:     MOPHWS, JFBMAX, XW
	 *=====================================================================
	 * GLOBAL OUTPUT;
	 *    GD2:     MFBUF, JFBPNT
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *   SACLIB:   FLUSHBUFFER2
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    831026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861017
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Write hardware text size opcode into SGF buffer. */
	Mfbuf[cmgd2.jfbpnt] = MOPHWS;
	Mfbuf[cmgd2.jfbpnt + 1] = 2;
	Mfbuf[cmgd2.jfbpnt + 2] = width*XW;
	Mfbuf[cmgd2.jfbpnt + 3] = height*XW;
	cmgd2.jfbpnt = cmgd2.jfbpnt + 4;

	/* - Flush buffer if necessary. */

	if( cmgd2.jfbpnt > JFBMAX )
		flushbuffer2( &nerr );

L_8888:
	return;

} /* end of function */

