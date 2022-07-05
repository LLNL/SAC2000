#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ begindevice(device, device_s, nerr)
char *device;   int device_s;
int *nerr;
{

	/*=====================================================================
	 * PURPOSE: To begin plotting to a graphics device.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    device:  Name of graphic device to "turn on". [c]
	 *             = 'TERMINAL' for Tektronix 4010/4014 type terminal.
	 *             = 'SGF' for SAC Graphics File.
	 *             = 'XWINDOWS' for X-window system.
	 *             = 'SUNWINDOWS' for SUN window system.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 0201.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  begindevices
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870416:  Reduced this subroutine to a call to BEGINDEVICES.
	 *    870127:  Added multi-windowing logic.
	 *    861010:  Major restructuring.
	 *    860116:  Fixed bug when a specific terminal type was entered.
	 *    831027:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870416
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	begindevices( device,device_s, 1, nerr );

L_8888:
	return;

} /* end of function */

