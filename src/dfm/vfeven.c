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



void /*FUNCTION*/ vfeven(nerr)
int *nerr;
{
	int ic1, ic2, jdfl, jdfl_, ndx1, ndx2, nlen;
	void *_p0;



	/*=====================================================================
	 * PURPOSE:  To verify that only evenly spaced time series files are in
	 *           the data file list (DFL).
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag.  Set to zero if no error occurs.
	 *             = 1306 if all files are not evenly spaced.
	 *             = 1307 if all files are not time series files.
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NDFL
	 *    HDR:     IFTYPE, ITIME, IXY, LEVEN
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  GTOUTM
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
		if( !*leven ){
			*nerr = 1306;
			setmsg( "ERROR", *nerr );
			lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			goto L_8888;
			}
		else if( *iftype == *irlim || *iftype == *iamph ){
			*nerr = 1307;
			setmsg( "ERROR", *nerr );
			lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			goto L_8888;
			}
                else if( *iftype == *ixyz ){
                        *nerr = 1378;
			setmsg( "ERROR", *nerr );
                        goto L_8888;
		        }
		}


L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    850322:  Fixed logic in checking for spectral files.
	 *    820622:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850322
	 *===================================================================== */

} /* end of function */

