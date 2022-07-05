#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void getxw(double ywloc, float* xwloc)
{
	int index, ioffst;



	/*=====================================================================
	 * PURPOSE: To get x data point from current data file given y value.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    YWLOC:   Y value in world coordinates. [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    XWLOC:   Corresponding x value in world coordinates. [f]
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
	ioffst = (ywloc - *b + 0.5**delta)/ *delta;

	/* - Compute starting location of current data file. */

	index = cmdfm.ndxdta[cmdfm.idflc - 1][0];

	/* - Return corresponding x world coordinate. */

	*xwloc = *(cmmem.sacmem[index]+ioffst);

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    961219:  Original version; copied from getyw.c,  maf.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  getyw.c was done on 850617
	 *===================================================================== */

} /* end of function */

