#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/sss.h"

void apcmsg2(char* kalpha, int kalpha_s);


void /*FUNCTION*/ vmdly(nerr)
int *nerr;
{
	int lmissd;
	int ic1, ic2, jdfl, jdfl_;
	float dstsq, t0vmsq, vappsq;
	void *_p0;



	/*=====================================================================
	 * PURPOSE:  To calculate delays for files in stack file list.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  sss/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    dfm:    ndfl
	 *    hdr:    fundef, kdfl
	 *    sss:    dst, ivm, inmo, irefr, vapp, t0vm, tvm
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    sss:    dlyvm
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  setmsg, lnumcl, apcmsg, apimsg
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    lmissd:   Flag used when checking for missing distances. [l]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881117:  Changed indexing of files in data file list.
	 *    860130:  Fixed bug when there were missing distances.
	 *    840806:  Allowed for "negative" distances in velocity models.
	 *    821207:  Documented subroutine.
	 *    810120:  Changed to output message retrieval from disk.
	 *    790831:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881117
	 *===================================================================== */
	/* PROCECURE: */
	*nerr = 0;

	/* - Check for traces with missing distances. */

	lmissd = FALSE;
	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		jdfl_ = jdfl - 1;
		if( Dst[jdfl] == cmhdr.fundef ){
			if( !lmissd ){
				*nerr = 5104;
				setmsg( "ERROR", *nerr );
				lmissd = TRUE;
				}
			lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			}
		}
	if( lmissd )
		goto L_8888;

	/* - Calculate delays. */

	/* -- Normal moveout delays. */
	if( Ivm[1] == cmsss.inmo ){
		vappsq = Vapp[1]*Vapp[1];
		t0vmsq = T0vm[1]*T0vm[1];
		for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
			jdfl_ = jdfl - 1;
			dstsq = Dst[jdfl]*Dst[jdfl];
			Dlyvm[jdfl] = cmsss.tvm[0][0] - sqrt( t0vmsq + dstsq/vappsq );
			}

		/* -- Refracted wave delays. */
		}
	else if( Ivm[1] == cmsss.irefr ){
		for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
			jdfl_ = jdfl - 1;
			Dlyvm[jdfl] = cmsss.tvm[0][0] - T0vm[1] - fabs( Dst[jdfl] )/
			 Vapp[1];
			}

		}
	else{
		*nerr = 5110;
		setmsg( "ERROR", *nerr );
		apimsg( Ivm[1] );
		goto L_8888;
		}

L_8888:
	return;

} /* end of function */

