#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ getyw(xwloc, ywloc)
double xwloc;
float *ywloc;
{
	int index, ioffst;



	/*=====================================================================
	 * PURPOSE: To get y data point from current data file given x value.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    XWLOC:   X value in world coordinates. [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    YWLOC:   Corresponding y value in world coordinates. [f]
	 *             Found by looking at current data file.
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NDXDTA, IDFLC
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Compute index offset into current array. */
	ioffst = (xwloc - *b + 0.5**delta)/ *delta;

	/* - Compute starting location of current data file. */

	index = cmdfm.ndxdta[cmdfm.idflc - 1][0];

	/* - Return corresponding y world coordinate. */

	*ywloc = *(cmmem.sacmem[index]+ioffst);

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    850617:  Minor change due to addition of new memory manager.
	 *    820317:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850617
	 *===================================================================== */

} /* end of function */

