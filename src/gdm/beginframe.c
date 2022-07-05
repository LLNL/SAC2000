#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gdm.h"
#include "gam.h"
#include "gem.h"
void beginSGFtemp ( int * nerr );
void zgetgd( char* name, int name_len );
void beginframe5(int* nerr);
void beginframe3(int* nerr);
void beginframe4(int* nerr);


void /*FUNCTION*/ beginframe(int lprint , int* nerr)
{
	int lany;
	

	/*=====================================================================
	 * PURPOSE: To begin a new graphics frame.
	 *=====================================================================
	 * INPUT ARGUMENT:
	 *    lprint:  If TRUE, establish the print option. [l]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL: gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lbegf, lgdon, icolor, iline, twidth, thgt
	 *    gam:     kgddef
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     lbegf
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - Must end frame by calling endframe
	 * - Must use begindevice and enddevice to turn graphics devices on and off.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getstatus, begingraphics, begindevices, calvspace,
	 *             beginframe1, beginframe2, beginframe3, beginframe4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    891002:  Deleted call to ztrmlg.
	 *    831026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870427
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - If no graphics device is open, try to open the terminal. */

	getstatus( "ANY", &lany );
	if( !lany ){
	    begindevice( kmgam.kgddef,9, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	/* - End last frame if not already done. */

	if( cmgdm.lbegf ){
	    endframe( FALSE , nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	/* - If requested, establish the print option. */
	if ( lprint ) {
	    cmgem.lprint = TRUE ;

	    /* SGF device needs to be on:  there are four cases:
	     *  1) SGF is already on:  do nothing.
	     *  2) Some other device is on:  turn SGF on temporarily.
	     *  3) No devices on; SGF is default:  turn on SGF.
	     *  4) No devices on; SGF is not default:  turn on default &
	     *                                         turn on SGF temporarily.
	     */
	    if ( !Lgdon[2] ) { /* if not case 1 */
		getstatus ( "ANY" , &lany ) ;

		if ( lany ) {       /* if some other device is running  (case 2) */
		    beginSGFtemp ( nerr ) ;
		    if ( *nerr != 0 )
			goto L_8888 ;
		    cmgem.lSGFtemp = TRUE ;
		}
		else {
		    zgetgd( kmgam.kgddef,9 );

		    /* case 3 or 4, turn on default device */
		    begindevices( kmgam.kgddef,9, 1, nerr );
		    if( *nerr != 0 )
			goto L_8888;

		    if ( strcmp ( kmgam.kgddef , "sgf     " ) &&  /* case 4 */
			 strcmp ( kmgam.kgddef , "SGF     " ) ) {
			beginSGFtemp ( nerr ) ;
			if ( *nerr != 0 )
			    goto L_8888 ;
			cmgem.lSGFtemp = TRUE ;
		    }
		}
	    } /* end if ( !Lgdon[2] ) */
	} /* end if ( lprint ) */


	/* - Perform begin frame action for each active device. */

	if( Lgdon[1] ){
	    beginframe1( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	/* - If SGF device is active, must also reset color, linestyle, etc.
	 *   because each SGF frame is in a separate disk file. */

	if( Lgdon[2] ){
	    beginframe2( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    setcolor2( cmgdm.icolor );
	    setlinestyle2( cmgdm.iline );
	    settextsize2( cmgdm.twidth, cmgdm.thgt );
	}

	if( Lgdon[3] ){
	    beginframe3( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	if( Lgdon[4] ){
	    beginframe4( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	if( Lgdon[5] ){
	    beginframe5( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	/* - Calculate viewspace which might have changed since last frame. */

	calvspace();

	/* - Set begin frame flag. */

	cmgdm.lbegf = TRUE;

L_8888:
	return;

} /* end of function */

