#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/tt.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ timeadj(rdist, atime, nerr)
double rdist;
float *atime;
int *nerr;
{
	char kttunit[9];



	/*=====================================================================
	 * PURPOSE:  To adjust the time relative to the reduced travel time
	 *           curves.  The time can be adjusted to a relative velocity
	 *           or a phase.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    atime:   
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  sss/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    tt :      
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  phaseadj, velocityadj
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * KNOWN BUGS:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920804:  Original version (alg)
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	*atime = 0.0;
	if( cmtt.lrdtt ){
		if( cmtt.nttrd == 1 ){
			velocityadj( cmtt.rdvel, cmtt.ttdist, rdist, atime, nerr );
			}
		else if( cmtt.nttrd == 2 ){
			phaseadj( Ltteven[cmtt.nrdph], Nttpt[cmtt.nrdph], cmmem.sacmem[Ndxttx[cmtt.nrdph]], 
			 cmmem.sacmem[Ndxtty[cmtt.nrdph]], Xttfirst[cmtt.nrdph], Xttdel[cmtt.nrdph], 
			 cmtt.ttdist, rdist, atime, nerr );
			}
		}

L_8888:
	return;

} /* end of function */

