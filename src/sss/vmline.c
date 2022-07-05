#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MP	100

#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/sss.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ vmline(dstmn, dstmx, nerr)
double dstmn, dstmx;
int *nerr;
{
	int j, j_, np;
	float d, dstdel, floatjunk, t[MP], t1line, t2line;

	float *const T = &t[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To draw curve of velocity model #2 on record section plots.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  sss/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    790917:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Compute number of data points needed for line. */

	if( Lvm[1] ){
		if( Ivm[1] == cmsss.irefr && Ivm[2] == cmsss.irefr ){
			np = 2;
			}
		else{
			np = MP;
			}
		}
	else{
		if( Ivm[2] == cmsss.irefr ){
			np = 2;
			}
		else{
			np = MP;
			}
		}

	/* - Set up evenly spaced x (distance) axis. */

	dstdel = (dstmx - dstmn)/(float)( np - 1 );
	cmgem.lxgen = TRUE;
	cmgem.xfirst = dstmn;
	cmgem.xdelta = dstdel;

	/* - Compute data points:
	 *   (1) Compute velocity model 1 delays.
	 *   (2) Compute velocity model 2 delays without reference time.
	 *   (3) Add the two delays (addition of delay 1 conteracts subtraction in rs plot.) */

	for( j = 1; j <= np; j++ ){
		j_ = j - 1;
		if( Lvm[1] ){
			if( Ivm[1] == cmsss.irefr ){
				t1line = cmsss.tvm[0][0] - (T0vm[1] + d/Vapp[1]);
				}
			else{
				t1line = cmsss.tvm[0][0] - (sqrt( T0vm[1]*T0vm[1] + 
				 d*d/(powi(Vapp[1],2)) ));
				}
			}
		else{
			t1line = 0.;
			}
		if( Ivm[2] == cmsss.irefr ){
			t2line = T0vm[2] + d/Vapp[2];
			}
		else{
			t2line = sqrt( T0vm[2]*T0vm[2] + d*d/(Vapp[2]*Vapp[2]) );
			}
		T[j] = t2line + t1line;
		d = d + dstdel;
		}

	/* - Draw line. */

	pldta( (float*)&floatjunk, t, np, 1, 1, nerr );
	if( *nerr != 0 )
		goto L_8888;

L_8888:
	return;

} /* end of function */

