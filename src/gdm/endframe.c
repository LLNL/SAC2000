#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gdm.h"
#include "gem.h"
#include "gd2.h"

void endSGFtemp ( int * nerr );

void /*FUNCTION*/ endframe(ldelay , nerr)
int ldelay ;
int *nerr;
{
	void endframe3(), endframe4(), endframe5();



	/*=====================================================================
	 * PURPOSE: To end the current graphics frame.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL: gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lbegf
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     lbegf
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - Must call beginframe to begin frame.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  endframe1, endframe2, endframe3, endframe4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *     891002:  Deleted call to ztrmlg.
	 *     831026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  831026
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Perform end frame action if necessary. */

	if( cmgdm.lbegf ){

	    if( Lgdon[1] ){
		endframe1( nerr );
		if( *nerr != 0 )
		    goto L_8888;
	    }

	    if( Lgdon[2] ){
		endframe2( nerr );
		if( *nerr != 0 )
		    goto L_8888;
	    }

	    if( Lgdon[3] ){
		endframe3( nerr );
		if( *nerr != 0 )
		    goto L_8888;
	    }

	    if( Lgdon[4] ){
		endframe4( nerr );
		if( *nerr != 0 )
		    goto L_8888;
	    }

	    if( Lgdon[5] ){
		endframe5( nerr );
		if( *nerr != 0 )
		    goto L_8888;
	    }

	    /* Take care of PRINT option. */
	    if ( cmgem.lprint ) {
		char command[ 2001 ] ;

		/* produce a post script (ps) file in the /tmp directory */
		sprintf ( command , "sgftops %s /tmp/sactemp.ps ; lpr " , kmgd2.kfilename ) ;
		if ( kmgem.kptrName[0] != '\0' ) {
		    strcat ( command , "-P " ) ;
		    strcat ( command , kmgem.kptrName ) ;
		}
		strcat ( command , " /tmp/sactemp.ps" ) ;
		system ( command ) ;
		if ( ldelay )
		    sleep( 6 ) ;

		if ( cmgem.lSGFtemp )
		    endSGFtemp ( nerr ) ;
	    }

	    cmgdm.lbegf = FALSE ;
	    cmgem.lframe = cmgemsav.lframe = TRUE ;
	    cmgem.lprint = cmgemsav.lprint = FALSE ;
	    kmgem.kptrName[0] = kmgemsav.kptrName[0] = '\0' ;

	}

L_8888:
	return;

} /* end of function */

