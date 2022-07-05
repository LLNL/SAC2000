#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/eam.h"
void /*FUNCTION*/ xeamc(index, nerr)
int index, *nerr;
{



	/*=====================================================================
	 * PURPOSE: To execute a MOD command given its index number.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    INDEX:   The index number of the command.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 0901.
	 *=====================================================================
	 * MODULE/LEVEL: EAM/1
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  GTOUTM, XOHPF, CHPF, XWHPF, XOAPF, CAPF, XAPK
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Jump to correct command based upon its index number. */

	switch( index ){
		case 1: goto L_100;
		case 2: goto L_200;
		case 3: goto L_300;
		case 4: goto L_400;
		case 5: goto L_500;
		case 6: goto L_600;
		}

	/* - Error return if bad index value. */

	*nerr = 901;
	setmsg( "ERROR", *nerr );
	apcmsg( "in XEAMC",9 );
	goto L_8888;

	/* - Command 01: OHPF */

L_100:
	xohpf( nerr );
	goto L_8888;

	/* - Command 02: CHPF */

L_200:
	chpf( nerr );
	goto L_8888;

	/* - Command 03: WHPF */

L_300:
	xwhpf( nerr );
	goto L_8888;

	/* - Command 04: OAPF */

L_400:
	xoapf( nerr );
	goto L_8888;

	/* - Command 05: CAPF */

L_500:
	capf( nerr );
	goto L_8888;

	/* - Command 06: APK */

L_600:
	xapk( nerr );
	goto L_8888;

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820000:  Original version.
	 *===================================================================== */

} /* end of function */

