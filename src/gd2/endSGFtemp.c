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

void /*FUNCTION*/ endSGFtemp ( int * nerr )
{

        /*=====================================================================
         * PURPOSE: To end plotting to SGF
         *=====================================================================
         * OUTPUT ARGUMENTS:
         *    nerr:    Error flag. Set to 0 if no error occurred.
         *             Potential error numbers: 0201.
         *=====================================================================
         * MODULE/LEVEL:  gd2/4
         *=====================================================================
         * GLOBAL INPUT:
         *    mach:
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

	if( Lgdon[2] ){
	    enddevice2( nerr );
	    if( *nerr != 0 )
		return ;
	    Lgdon[2] = FALSE;
	}

	strcpy ( kmgd2.kfdir , kmgd2.kfdirStore ) ;
	cmgd2.nfdir = cmgd2.nfdirStore ;

	cmgdm.lbegf = FALSE ;
	cmgem.lSGFtemp = cmgemsav.lSGFtemp = FALSE ;
	cmgem.lframe = cmgemsav.lframe = TRUE ;
	cmgem.lprint = cmgemsav.lprint = FALSE ;
	kmgem.kptrName[0] = kmgemsav.kptrName[0] = '\0' ;

	/* - Calculate new values for graphics device status variables. */
	calstatus();
}
