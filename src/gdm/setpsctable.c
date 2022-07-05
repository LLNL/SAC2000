#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
#include "../../inc/gam.h"

void setpsctable3(int* win_num, unsigned nentry, float* red, float* green, float* blue);
void setpsctable5(int* win_num, unsigned nentry, float* red, float* green, float* blue);

void setpsctable(nerr)
int *nerr;
{

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

       	if( Lgdon[1] ){
          *nerr = 1;
          goto L_8888;
	}
       	if( Lgdon[2] ){
          *nerr = 2;
          goto L_8888;
       	      }
       	if( Lgdon[3] ) {
       		setpsctable3( &cmgdm.iwindow, cmgdm.nctsize + 1, 
       		 &cmgdm.ctred[0], &cmgdm.ctgreen[0], &cmgdm.ctblue[0] );
               /*        cmgam.cmap = MCOLOR; */
       	      }
       	if( Lgdon[4] ){
          *nerr = 4;
          goto L_8888;
	}
       	if( Lgdon[5] ) {
       		setpsctable5( &cmgdm.iwindow, cmgdm.nctsize + 1, 
       		 &cmgdm.ctred[0], &cmgdm.ctgreen[0], &cmgdm.ctblue[0] );
                       cmgam.cmap = MCOLOR;
       	      }


L_8888:
	return;

} /* end of function */

