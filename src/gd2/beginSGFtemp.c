#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gdm.h"
#include "gem.h"
#include "gd2.h"

void /*FUNCTION*/ beginSGFtemp ( int * nerr )
{

        /*=====================================================================
         * PURPOSE: To begin plotting to a list of graphics devices.
         *=====================================================================
         * OUTPUT ARGUMENTS:
         *    nerr:    Error flag. Set to 0 if no error occurred.
         *             Potential error numbers: 0201.
         *=====================================================================
         * MODULE/LEVEL:  gd2/4
         *=====================================================================
         * GLOBAL INPUT:
         *    mach:
         *    gdm:     mgd, iwindow, skdevfudge
         *=====================================================================
         * GLOBAL OUTPUT:
         *    gdm:     lgdon
         *=====================================================================
         * MODIFICATION HISTORY:
         *    981123:  Original version.
         *=====================================================================
         * DOCUMENTED/REVIEWED:
         *===================================================================== */
        /* PROCEDURE: */
        *nerr = 0;

        /* - Initialize the graphics library if needed. */

        if( !cmgdm.lginit ){
            begingraphics( nerr );
            if( *nerr != 0 )
                return ;
        }


	begindevice2( nerr );
	if( *nerr != 0 )
	    return ;
	Lgdon[2] = TRUE;
	cmgem.skdevfudge = 0.0003;

	strcpy ( kmgd2.kfdir , "/tmp" ) ;
	cmgd2.nfdir = 4 ;

	/* - Begin plotting to the current graphics window. */
	if( cmgdm.lgui ) cmgdm.iwindow = 1; /* only one window supported now. */
	beginwindow( cmgdm.iwindow, nerr );

	/* - Calculate new values for graphics device status variables. */
	calstatus();
}
