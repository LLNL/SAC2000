#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"

void enddevice3(int* nerr);
void enddevice4(int* nerr);
void enddevice5(int* nerr);


void enddevice(char *device, int device_s, int* nerr)
{
	int igd;



	/*=====================================================================
	 * PURPOSE: To end plotting to a graphics device.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    device:  The name of the device. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL: gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lbegf, lgdon
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     lgdon
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  endframe, endgraphics, enddevice1, enddevice2, enddevice3,
	 *             enddevice4, setmsg, apcmsg, calstatus
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    880520:  Fixed bug in call to endgraphics.
	 *    861010:  Major restructuring.
	 *    831027:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  879426
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - End current frame if necessary. */

	if( cmgdm.lbegf ){
		endframe( FALSE , nerr );
		if( *nerr != 0 )
			goto L_8888;
	}

	/* - Handle request to end all graphics devices here. Terminate library. */

	if( memcmp(device,"ALL",3) == 0 ){
		endgraphics( nerr );
		if( *nerr != 0 )
			goto L_8888;
	}

	/* - Check name versus list of graphics devices. */
	else if( lequal( device,device_s, (char*)kmgdm.kgdnam,13, MGD, &igd ) ){

		if( igd == 1 ){
			if( Lgdon[1] ){
				enddevice1( nerr );
				if( *nerr != 0 )
					goto L_8888;
				Lgdon[1] = FALSE;
			}
		}

		else if( igd == 2 ){
			if( Lgdon[2] ){
				enddevice2( nerr );
				if( *nerr != 0 )
					goto L_8888;
				Lgdon[2] = FALSE;
			}
		}

		else if( igd == 3 ){
			if( Lgdon[3] ){
				enddevice3( nerr );
				if( *nerr != 0 )
					goto L_8888;
				Lgdon[3] = FALSE;
			}
		}

		else if( igd == 4 ){
			if( Lgdon[4] ){
				enddevice4( nerr );
				if( *nerr != 0 )
					goto L_8888;
				Lgdon[4] = FALSE;
			}
		}

		else if( igd == 5 ){
			if( Lgdon[5] ){
				enddevice5( nerr );
				if( *nerr != 0 )
					goto L_8888;
				Lgdon[5] = FALSE;
			}
		}

		else{
			*nerr = 901;
			setmsg( "ERROR", *nerr );
			apcmsg( "in ENDDEVICE",13 );
			goto L_8888;
		}
	}

	/*   Raise illegal device error if still no match. */
	else{
		*nerr = 201;
		setmsg( "ERROR", *nerr );
		apcmsg( device,device_s );
		goto L_8888;
	}

	/* - Calculate new values for graphics device status variables. */

	calstatus();

L_8888:
	return;

} /* end of function */

