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


void /*FUNCTION*/ vfrng(rngmin, rngmax, nerr)
double rngmin, rngmax;
int *nerr;
{
	int ic1, ic2, jdfl, jdfl_, ndx1, ndx2, nlen;
	void *_p0;



	/*=====================================================================
	 * PURPOSE:  To verify that the dependent variables for all files
	 *           in the DFL are in the allowed range.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    RNGMIN:  Minimum value allowed.
	 *    RNGMAX:  Maximum value allowed.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag.  Set to zero if no error occurs.
	 *             Set to 1340 if all files are not in allowed range.
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NDFL
	 *    HDR:     DEPMIN, DEPMAX
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  GETFIL, GTOUTM
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - For each file in DFL: */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		jdfl_ = jdfl - 1;

		/* -- Get header from memory manager. */
		getfil( jdfl, FALSE, &nlen, &ndx1, &ndx2, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Check range of dependent variable. */
		if( *depmin < rngmin || *depmax > rngmax ){
			*nerr = 1340;
			setmsg( "ERROR", *nerr );
			apfmsg( rngmin );
			apfmsg( rngmax );
			lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			goto L_8888;
			}

		}


L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820818:  Original version.
	 *===================================================================== */

} /* end of function */

