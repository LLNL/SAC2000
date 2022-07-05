#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ kadttm(idttm, kdttm, kdttm_s, nerr)
int idttm[];
char *kdttm;   int kdttm_s;
int *nerr;
{
	char kdt[19], ktm[13];

	int *const Idttm = &idttm[0] - 1;



	/*=====================================================================
	 * PURPOSE: To convert an integer date/time array to it's
	 *          alphanumeric equivalent.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    idttm:   Six element date/time array. [ia=6]
	 *             Elements are year, julian day, hour, minute,
	 *             second, and millisecond.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    kdttm:   Alphanumeric date/time equivalent. [c]
	 *             Should be at least 32 characters int.
	 *             Form is: MMM DD (JJJ), YYYY  HH:MM:SS.S
	 *    nerr:    Error return flag. [i]
	 *             Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL: service/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:    kadate, katime
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900502:  Forced length of kdttm to be 32 characters int.
	 *    860203:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860203
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Convert date part. */
	kadate( Idttm[1], Idttm[2], 18, kdt,19, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Convert time part. */

	katime( Idttm[3], Idttm[4], Idttm[5], Idttm[6], 12, ktm,13, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Append two parts to form output argument. */

        fstrncpy( kdttm, kdttm_s-1, kdt, strlen(kdt));
        fstrncpy( kdttm+strlen(kdt), kdttm_s-1-strlen(kdt),
                                           " ", 1 );
        fstrncpy( kdttm+strlen(kdt)+1, kdttm_s-1-strlen(kdt)-1,
                                    ktm, strlen(ktm));


L_8888:

	return;

} /* end of function */

