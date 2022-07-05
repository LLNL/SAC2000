#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ xcolor(nerr)
int *nerr;
{
	char ktok[9];
	int lnum, log;
	int inum;
	float rnum;



	/*=====================================================================
	 * PURPOSE: To parse the parameter-setting command COLOR.
	 *          COLOR controls the color display attributes.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     MICOL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     LCOL, LICOL, ICOL, ISKCOL, IBACOL, IICOL, NICOL, JICOL
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - CONVCOLORNAME converts the name of standard color to a color
         *                 index value.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCLOG, LCTOK, ICTOK
	 *             CONVCOLORNAME
	 *=====================================================================
	 * KNOWN ERRORS:
	 * - Not sending error message if user attempts to create a
	 *   too large a color list.  The last color in list is changed.
	 *=================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Parse position-dependent tokens: */

	if( lclog( &cmgem.lcol ) )
        { /* do nothing */ }
	else if( lctok( ktok,9, &lnum, &rnum ) ){
	    inum = (int)( rnum + 0.1 );
	    if( lnum ){
		cmgem.icol = inum;
		cmgem.lcol = TRUE;
		ictok( 1 );
	    }
	    else{
		convcolorname( ktok, &inum );
		if( inum >= 0 ){
		    cmgem.icol = inum;
		    cmgem.lcol = TRUE;
		    ictok( 1 );
		}
	    }
	}

	/* - Parse position-independent tokens: */

	while( lcmore( nerr ) ){

	    /* -- "SKELETON color/int":  change skeleton color. */
	    if( lckey( "SK$",4 ) ){
		log = lctok( ktok,9, &lnum, &rnum );
		inum = (int)( rnum + 0.1 );
		if( lnum ){
		    cmgem.iskcol = inum;
		    ictok( 1 );
		}
		else{
		    convcolorname( ktok, &inum );
		    if( inum >= 0 ){
			cmgem.iskcol = inum;
			ictok( 1 );
		    }
		    else{
			cfmt( "NEED NAME OF A COLOR:$",23 );
			cresp();
			ictok( -1 );
		    }
		}
	    }

	    /* -- "BACKGROUND color/int":  change the background color. */
	    else if( lckey( "BA$",4 ) ){
		log = lctok( ktok,9, &lnum, &rnum );
		inum = (int)( rnum + 0.1 );
		if( lnum ){
		    cmgem.ibacol = inum;
		    ictok( 1 );
		}
		else{
		    convcolorname( ktok, &inum );
		    if( inum >= 0 ){
			cmgem.ibacol = inum;
			ictok( 1 );
		    }
		    else{
			cfmt( "NEED NAME OF A COLOR:$",23 );
			cresp();
			ictok( -1 );
		    }
		}
	    }

	    /* -- "LIST STANDARD/colorlist":  change the color list. */
	    else if( lckey( "L$",3 ) ){
		if( lckey( "S$",3 ) ){
		    inicol( cmgem.iicol, &cmgem.nicol );
		}
		else{
		    cmgem.nicol = 0;
		    while( lctok( ktok,9, &lnum, &rnum ) ){
			inum = (int)( rnum + 0.1 );
			if( lnum ){
			    if( cmgem.nicol < MICOL )
				cmgem.nicol = cmgem.nicol + 1;
			    Iicol[cmgem.nicol] = inum;
			    ictok( 1 );
			}
			else{
			    convcolorname( ktok, &inum );
			    if( inum >= 0 ){
				if( cmgem.nicol < MICOL )
				    cmgem.nicol = cmgem.nicol + 1;
				Iicol[cmgem.nicol] = inum;
				ictok( 1 );
			    }
			}
		    }
		    if( cmgem.nicol <= 0 )
			inicol( cmgem.iicol, &cmgem.nicol );
		    cmgem.icol = Iicol[1];
		    cmgem.lcol = TRUE;
		    cmgem.jicol = 0;
		}
	    }

	    /* -- "INCREMENT ON/OFF":  increment color after each file or not */
	    else if( lklog( "I$",3, &cmgem.licol ) ){
		cmgem.lcol = TRUE;
		cmgem.jicol = 0;
	    }

	    /* -- Bad syntax. */
	    else{
		cfmt( "ILLEGAL OPTION:$",17 );
		cresp();

	    }
	}  /* end while( lcmore( nerr ) ) */

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    821221:  Added ability to change color list.
	 *    820809:  Changed to newest set of parsing and checking functions.
	 *    820305:  Original version.
	 *================================================================== */

} /* end of function */

