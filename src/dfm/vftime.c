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


void /*FUNCTION*/ vftime(nerr)
int *nerr;
{
	int ic1, ic2, jdfl, jdfl_, ndx1, ndx2, nlen;
	void *_p0;



	/*=====================================================================
	 * PURPOSE:  To verify that only time-series files are in the data file list.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag.  Set to zero if no error occurs.
	 *             Set to 1307 if all files are not time series files.
	 *=====================================================================
	 * MODULE/LEVEL:  dfm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    dfm:     ndfl
	 *    hdr:     iftype, itime, ixy, ixyz
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     getfil, setmsg, lnumcl, apcsmg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900503:  Added check for xyz data file type.
	 *    820622:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900305
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

		/* -- Check file type. */
		if( (*iftype != *itime && *iftype != *ixy) && *iftype != *ixyz ){
			*nerr = 1307;
			setmsg( "ERROR", *nerr );
			lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			goto L_8888;
			}

		}


L_8888:
	return;

} /* end of function */

