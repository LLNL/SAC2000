#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ vfmax(maxf, nerr)
int *maxf, *nerr;
{
	int ic1, ic2, jdfl, jdfl_, ndx1, ndx2, nlen;
	void *_p0;



	/*=====================================================================
	 * PURPOSE:
	 *         
	 *           The maximum number of data points found is returned.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 * 
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    MAXF:    Maximum number of data points found in files.
	 *    NERR:    Error return flag.  Set to zero if no error occurs.
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NDFL
	 *    HDR:     NPTS
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  GTOUTM
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	*maxf = 0;

	/* - For each file in DFL: */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		jdfl_ = jdfl - 1;

		/* -- Get header from memory manager. */
		getfil( jdfl, FALSE, &nlen, &ndx1, &ndx2, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- See if this files number exceeds maximum found so far. */
		if( *npts > *maxf )
			*maxf = *npts;


		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820622:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  820622
	 *===================================================================== */

} /* end of function */

