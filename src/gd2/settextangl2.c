#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gd2.h"
void /*FUNCTION*/ settextangle2(angle)
double angle;
{
	int nerr;



	/*=====================================================================
	 * PURPOSE:  To change the textangle for graphics device 2 (SGF).
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ANGLE:   Set Text Angle.
	 *=====================================================================
	 * MODULE/LEVEL:  GD2/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GD2:     MOPLIN
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  FLUSHBUFFER2
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861020:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861020
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Write linestyle opcode to SGF buffer. */
	Mfbuf[cmgd2.jfbpnt] = MOPTAN;
	Mfbuf[cmgd2.jfbpnt + 1] = 1;
	Mfbuf[cmgd2.jfbpnt + 2] = angle;
	cmgd2.jfbpnt = cmgd2.jfbpnt + 3;

	/* - Flush buffer if necessary. */

	if( cmgd2.jfbpnt > JFBMAX )
		flushbuffer2( &nerr );

L_8888:
	return;

} /* end of function */

