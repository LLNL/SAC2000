


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/eam.h"
void /*FUNCTION*/ whpf1(koutm, koutm_s)
char *koutm;   int koutm_s;
{
	char kampx[5], kfmp[9], kprx[4], kssecs[6];
	int lampl;
	int npyr;

	/*=====================================================================
	 * PURPOSE:  To put the current pick into the HYPO pick format.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KOUTM:   String containing the HYPO formatted pick [char*80].
	 *=====================================================================
	 * MODULE/LEVEL:  EAM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  CNVFTA
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *===================================================================== */
	/* PROCEDURE: */
	/* - If there is no P-wave pick, do nothing. */
	if( !cmeam.lpphas ){
		fstrncpy( koutm, koutm_s-1, " ", 1 );
		goto L_8888;
		}

	/* - Set up S-wave output fields if applicable. */

	if( cmeam.lsphas ){
                sprintf(kssecs,"%5.2f",cmeam.ssecs);
		}
	else{
		strcpy( kssecs, "     " );
		strcpy( kmeam.kswave, "        " );
		}

	/* - Set up maximum amplitude and period fields if applicable. */

	if( lampl ){
		cnvfta( cmeam.ampx, 4, 0, kampx,5 );
		ljust( kampx,5 );
		cnvfta( cmeam.prx, 3, 2, kprx,4 );
		ljust( kprx,4 );
		}
	else{
		strcpy( kampx, "    " );
		strcpy( kprx, "   " );
		}

	/* - Set up duration field if applicable. */

	if( cmeam.lfini ){
		strcpy( kfmp, "        " );
		cnvfta( cmeam.fmp, 5, 1, kfmp,9 );
		ljust( kfmp,9 );
		}
	else{
		strcpy( kfmp, "        " );
		}

	/* - Make sure year has only two digits. */

	npyr = cmeam.npyear - 100*(cmeam.npyear/100);

	/* - Write the HYPO formatted line to the output character string. */

        sprintf(koutm,"%.4s%.4s %2d%2d%2d%2d%2d%5.2f       %.5s%.4s   \
%.4s%.3s                    %.5s",
	 kmeam.kstid, kmeam.kpwave, npyr, cmeam.npmon, cmeam.npday, cmeam.nphour, 
	 cmeam.npmin, cmeam.psecs, kssecs, kmeam.kswave, kampx, kprx, 
	 kfmp );

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820810:  Changed names for HPF and APF variables.
	 *    810909:  Fixed bug in converting SSECS to ASCII.
	 *===================================================================== */

} /* end of function */

