#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ copykc(kin, kin_s, ncopy, kout )
char *kin;   int kin_s;
int ncopy;
char *kout;
{
#define KIN(I_,J_)	(kin+(I_)*(kin_s)+(J_))
	int icin, icout, icout_, iwin;

	/*=====================================================================
	 * PURPOSE:  To copy a character array into a character string.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kin:     Character array. [ka]
	 *    ncopy:   Number of characters to copy. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    kout:    Character string. [c]
	 *=====================================================================
	 * MODULE/LEVEL:  service/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCPW
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    icin:    Character index into input character array. [i]
	 *    iwin:    Element index into input character array. [i]
	 *    icout:   Character index into output character string. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900517:  Corrected declaration for kin to fix VAX VMS problem.
	 *    830810:  Cleaned up and converted to indepenent subroutine.
	 *    810000:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900517
	 *===================================================================== */
	/* PROCEDURE: */
	/* - KIN is a character array, each element being MCPW characters int.
	 *   KOUT is a character string of arbitrary length. */
	iwin = 1;
	icin = 1;

	/* - For each character being copied. */

	for( icout = 1; icout <= ncopy; icout++ ){
		icout_ = icout - 1;

		/* -- Copy character to output string. */
		kout[icout - 1] = KIN(iwin - 1,0)[icin - 1];

		/* -- Increment input character and array counters. */
		icin = icin + 1;
		if( icin > MCPW ){
			iwin = iwin + 1;
			icin = 1;
			}
		}

L_8888:
	return;

#undef	KIN
} /* end of function */

