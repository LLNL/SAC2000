#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gd2.h"
#include "msg.h"


void /*FUNCTION*/ xprint ( int *nerr ) 
{
	char command[ 2001 ] , printerName[ 80 ];
	int unused ;
	int lprinter = FALSE ;


        /*=====================================================================
         * PURPOSE:  To execute the action command PRINT 
         *           This command makes a hardcopy of the last SGF file produced
         *=====================================================================
         * OUTPUT ARGUMENTS:
         *    nerr:    Error return flag.  Set to 0 if no error occurred.
         *=====================================================================
         * MODULE/LEVEL:  gam/2
         *=====================================================================
         * GLOBAL INPUT:
	 *   GD2:
	 *	kmgd2.kfilename
         *=====================================================================
         * SUBROUTINES CALLED:
	 *  CPF:
	 *	lcchar
         *=====================================================================
         * MODIFICATION HISTORY:
 	 *	999422:	Original version.  
         *=====================================================================

         PROCEDURE: */
        *nerr = 0;

        /* PARSING PHASE: */

        /* - There will be one or fewer tokens:  printer name */

	if ( lcchar ( 79 , printerName , 80 , &unused ) )
	    lprinter = TRUE ;
	    
	/* EXECUTION PHASE */

	if ( kmgd2.kfilename[ 0 ] ) {	/* if there is an SGF file */
	    /* produce a post script (ps) file in the /tmp directory */
	    sprintf ( command , "sgftops %s /tmp/sactemp.ps ; lpr " , kmgd2.kfilename ) ;
	    if ( lprinter ) {
		strcat ( command , "-P " ) ;
		strcat ( command , printerName ) ;
	    }
	    strcat ( command , " /tmp/sactemp.ps" ) ;
	    system ( command ) ;
	}
	else 		/* if no SGF files have been produced */
	    *nerr = 2405 ;
}
