#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
#include "../../inc/gam.h"

void /*FUNCTION*/ beginwindow(number, nerr)
int number, *nerr;
{
	int exists;
	int _l0;
	float screenratio, ymin, ymax;
	void beginwindow3(), beginwindow4(), setctable3(), setctable4();
        void beginwindow5(), setctable5();

	/*=====================================================================
	 * PURPOSE: To begin plotting to a graphics window.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    number:  Number of graphic window to "turn on". [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 0201.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     mwindows, lgdon, nctsize, ctred, ctgreen, ctblue
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     iwindow
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getwindowstatus, createwindow, 
	 *             setctable1, setctable2, setctable3, setctable4, setcolor,
	 *             beginwindow1, beginwindow2, beginwindow3, beginwindow4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900829;  Fixed bug created by last bug fix, call setctable with
	 *             nctsize+1. [wct]
	 *    900817:  Fixed bug created by last change. [jet]
	 *    900803:  Changed method of setting color table for new window. 
	 *    870426:  Moved window checking and creation from begindevices.
	 *    861201:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850506
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Check input window number for correctness. */

	cmgdm.iwindow = max( 1, min( MWINDOWS, number ) );
    
        if( cmgdm.lgui ) cmgdm.iwindow = 1; /* GUI only supports one window for now. */
	/* - Create a new graphics window if necessary. */

	getwindowstatus( &cmgdm.iwindow, &exists );
	if( !exists ){
		getdeviceratio( &screenratio );
                if( Lgdon[3] || Lgdon[5] ){
                        ymin = Ywindowmin[cmgdm.iwindow];
                        ymax = Ywindowmax[cmgdm.iwindow];
		}else{
                        ymin = Ywindowmin[cmgdm.iwindow]*screenratio;
                        ymax = Ywindowmax[cmgdm.iwindow]*screenratio;
		}
		createwindow( &cmgdm.iwindow, Xwindowmin[cmgdm.iwindow], Xwindowmax[cmgdm.iwindow], 
		        ymin, ymax, nerr );
		if( *nerr != 0 )
			goto L_8888;
		if( Lgdon[1] )
			setctable1( cmgdm.iwindow, cmgdm.nctsize + 1, cmgdm.ctred[0], 
			 cmgdm.ctgreen[0], cmgdm.ctblue[0] );

		if( Lgdon[2] ){
			setctable2( cmgdm.iwindow, cmgdm.nctsize + 1, cmgdm.ctred[0], 
			 cmgdm.ctgreen[0], cmgdm.ctblue[0] );
                        cmgam.cmap = MCOLOR;
		      }
		if( Lgdon[3] ) {
			setctable3( &cmgdm.iwindow, cmgdm.nctsize + 1, 
			 &cmgdm.ctred[0], &cmgdm.ctgreen[0], &cmgdm.ctblue[0] );
                        cmgam.cmap = MDEFAULT;
		      }
		if( Lgdon[4] )
			setctable4( &cmgdm.iwindow, cmgdm.nctsize + 1, 
			 &cmgdm.ctred[0], &cmgdm.ctgreen[0], &cmgdm.ctblue[0] );
		if( Lgdon[5] ) {
			setctable5( &cmgdm.iwindow, cmgdm.nctsize + 1, 
			 &cmgdm.ctred[0], &cmgdm.ctgreen[0], &cmgdm.ctblue[0] );
                        cmgam.cmap = MDEFAULT;
		      }
	      }

	/* - Activate the graphics window for each active device. */

	if( Lgdon[1] ){
		beginwindow1( cmgdm.iwindow, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[2] ){
		beginwindow2( cmgdm.iwindow, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[3] ){
		beginwindow3( &cmgdm.iwindow, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[4] ){
		beginwindow4( &cmgdm.iwindow, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[5] ){
		beginwindow5( &cmgdm.iwindow, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	/* - Set default color if window did not exist.
	 *   Must be done after beginwindow, at least for Sunwindows. */
	if( !exists )
		setcolor( cmgdm.nctsize );

	/* - Calculate new values for graphics status variables. */

	calstatus();

L_8888:
	return;

} /* end of function */

