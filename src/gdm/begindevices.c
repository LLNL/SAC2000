#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gdm.h"
#include "gem.h"

void endSGFtemp ( int * nerr );


void /*FUNCTION*/ begindevices(char* devices, int devices_s, int ndevices, int* nerr)
{
#define DEVICES(I_,J_)	(devices+(I_)*(devices_s)+(J_))
	char ktype[9];
	int turnon[MGD];
	int igd, jdevice, jgd;
	void begindevice3(), begindevice4(), enddevice3(), enddevice4();
        void begindevice5(), enddevice5();

	int *const Turnon = &turnon[0] - 1;


	/*=====================================================================
	 * PURPOSE: To begin plotting to a list of graphics devices.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    devices:  List of graphic devices to "turn on". [ca]
	 *              Legal graphics devices names are:
	 *              = 'TERMINAL' for Tektronix 4010/4014 type terminal.
	 *              = 'SGF' for SAC Graphics File.
	 *              = 'XWINDOWS' for X-window system.
	 *              = 'SUNWINDOWS' for SUN window system.
         *              = 'GUI' for graphical interface.
	 *    ndevices: Number of devices in list. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 0201.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     mgd, iwindow, skdevfudge
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     lgdon
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  begingraphics, setmsg, apcmsg, 
	 *             begindevice1, enddevice1, begindevice2, enddevice2,
	 *             begindevice3, enddevice3, begindevice4, enddevice4, 
	 *             beginwindow, calstatus
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920604:  Added skdevfudge fudge factor (skeleton line adjustment)
	 *             for device 3. Used with WIDTH option.
	 *    910508:  Changed "device" to "devices" in call apcmsg(device).
	 *             When an illegal device is requested, "device" causes 
	 *             a segmentation fault, must have been a typo (wct).
	 *    870426:  Moved window checking and creation to beginwindow.
	 *    870127:  Added multi-windowing logic.
	 *    861010:  Major restructuring.
	 *    860116:  Fixed bug when a specific terminal type was entered.
	 *    831027:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850506
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Initialize the graphics library if needed. */

	if( !cmgdm.lginit ){
	    begingraphics( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	/* - Initialize "turn-on" flag for each device to .FALSE. */

	for( igd = 1; igd <= MGD; igd++ ){
	    Turnon[igd] = FALSE;
	}

	/* - Check each input device name for correctness. */

	for( jdevice = 0; jdevice < ndevices; jdevice++ ){

	    /* if XWINDOWS device was requested and GUI is active change device */
	    /*   name to GUI to ensure mutual exclusivity.                      */ 
	    if( cmgdm.lgui )
		if(( *(DEVICES(jdevice,0)) == 'x') || ( *(DEVICES(jdevice,0)) == 'X')   ){
		    memset(DEVICES(jdevice,0),' ',devices_s-1);
		    strncpy(DEVICES(jdevice,0),"GUI",3);
		}

	    /* -- Check name versus list of graphics devices. */
	    if( lequal( DEVICES(jdevice,0),devices_s, (char*)kmgdm.kgdnam
	     ,13, MGD, &jgd ) ){
		Turnon[jgd] = TRUE;
	    }
	} /* end for */

	/* Abandon SGF if it's on temporarily for PRINT option. */
	if ( cmgem.lSGFtemp )
	    endSGFtemp ( nerr ) ;

	/* - For each graphics device:
	 *   (1) If has been requested and is not currently active, turn it on.
	 *   (2) If not requested and is currenly active, turn it off.
	 *   (3) Otherwise, do nothing. */

/*	if( Turnon[1] && !Lgdon[1] ){
	    begindevice1( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    Lgdon[1] = TRUE;
	    cmgdm.igdtxt = 1;
	}
	else if( !Turnon[1] && Lgdon[1] ){
	    enddevice1( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    Lgdon[1] = FALSE;
	}
*/
	if( Turnon[2] && !Lgdon[2] ){
	    begindevice2( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    Lgdon[2] = TRUE;
	    cmgem.skdevfudge = 0.0003;
	}
	else if( !Turnon[2] && Lgdon[2] ){
	    enddevice2( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    Lgdon[2] = FALSE;
	}

	if( Turnon[3] && !Lgdon[3] ){
	    begindevice3( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    Lgdon[3] = TRUE;
	    cmgdm.igdtxt = 3;
	    cmgem.skdevfudge = 0.00095;
	}
	else if( !Turnon[3] && Lgdon[3] ){
	    enddevice3( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    Lgdon[3] = FALSE;
	}

	if( Turnon[4] && !Lgdon[4] ){
	    begindevice4( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    Lgdon[4] = TRUE;
	    cmgdm.igdtxt = 4;
	}
	else if( !Turnon[4] && Lgdon[4] ){
	    enddevice4( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    Lgdon[4] = FALSE;
	}

	if( Turnon[5] && !Lgdon[5] ){
	    begindevice5( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    Lgdon[5] = TRUE;
	    cmgdm.igdtxt = 3;
	    cmgem.skdevfudge = 0.00095;
	}
	else if( !Turnon[5] && Lgdon[5] ){
	    enddevice5( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    Lgdon[5] = FALSE;
	}


	/* - Begin plotting to the current graphics window. */

        if( cmgdm.lgui ) cmgdm.iwindow = 1; /* only one window supported now. */
	beginwindow( cmgdm.iwindow, nerr );

	/* - Calculate new values for graphics device status variables. */

	calstatus();

L_8888:
	return;

#undef	DEVICES
} /* end of function */

