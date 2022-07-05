#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"

void apcmsg2(char* kalpha, int kalpha_s);


void /*FUNCTION*/ vfmaxn(maxn, maxf, nerr)
int maxn, *maxf, *nerr;
{
	int ic1, ic2, jdfl, jdfl_, ndx1, ndx2, nlen;
	void *_p0;



	/*=====================================================================
	 * PURPOSE:  To verify that no files in data file list have more
	 *           a certain number of data points.
	 *           The maximum number of data points found is returned.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    MAXN:    Maximum number of data points that can be in each file.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    MAXF:    Maximum number of data points found in files.
	 *    NERR:    Error return flag.  Set to zero if no error occurs.
	 *             Set to 1338 if one or more files exceed maximum
	 *             number of data points.
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

		/* -- Check number of data points versus maximum allowable. */
		if( *npts > maxn ){
			*nerr = 1338;
			setmsg( "ERROR", *nerr );
			apimsg( maxn );
			lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			goto L_8888;
			}

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

