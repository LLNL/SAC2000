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


void /*FUNCTION*/ vfxyz(nerr)
int *nerr;
{
	int ic1, ic2, jdfl, jdfl_, ndx1, ndx2, nlen;
	void *_p0;



	/*=====================================================================
	 * PURPOSE:  To verify that only xyz data files are in data file list.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag.  Set to zero if no error occurs.
	 *             Set to 1366 if all files are not xyz data files.
	 *=====================================================================
	 * MODULE/LEVEL:  xyz/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    dfm:     ndfl, kdfl
	 *    hdr:     iftype, ixyz
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     getfil, setmsg, lnumcl, apcmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900305:  Original version.
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
		if( *iftype != *ixyz ){
			*nerr = 1366;
			setmsg( "ERROR", *nerr );
			lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			goto L_8888;
			}

		}


L_8888:
	return;

} /* end of function */

