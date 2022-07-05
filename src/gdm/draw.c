#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"

#define	IABOVE	8
#define	IBELOW	4
#define	ILEFT	1
#define	INSIDE	0
#define	IRIGHT	2

#include "mach.h"
#include "gdm.h"


void draw5(float* xloc_vp, float* yloc_vp);
void move5(float* xloc_vp, float* yloc_vp);


void draw(float xloc, float yloc)
{
	int iloc, ixyloc[2], unused;
	float xline[2], yline[2];
	void draw3(), draw4(), move3(), move4();
        float xlocs, ylocs;

	int *const Ixyloc = &ixyloc[0] - 1;
	float *const Xline = &xline[0] - 1;
	float *const Yline = &yline[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To draw from the current point to the requested location.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    xloc:    X viewspace location. [f]
	 *    yloc:    Y viewspace location. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lvsclip, xvs, yvs, lgdon
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     xold, yold, iold
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *   saclib:   draw1, draw2, draw3, draw4, clipdp,
	 *             move1, move2, move3, move4
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *   xline:    X locations of line from current point to input point.
	 *   yline:    Y locations of line from current point to input point.
	 *   ixyloc:   Locations of line segment relative to viewspace.
	 *   iabove:   Value indicating a point is above the viewspace.
	 *   ibelow:   Value indicating a point is below the viewspace.
	 *   iright:   Value indicating a point is to right of viewspace.
	 *   ileft:    Value indicating a point is to left of viewspace.
	 *   inside:   Value indicating a point is inside viewspace.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *   870501:   Added option to disable viewspace clipping.
	 *   860910:   Added viewspace clipping.
	 *   830523:   Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870423
	 *===================================================================== */
	/* PROCEDURE: */

        xlocs = xloc;
        ylocs = yloc;

	/* - If viewspace clipping is off, send draw to active devices. */
	if( !cmgdm.lvsclip ){

	    if( Lgdon[1] )
		draw1( &xlocs, &ylocs );
	    if( Lgdon[2] )
		draw2( xloc, yloc );
	    if( Lgdon[3] )
		draw3( &xlocs, &ylocs );
	    if( Lgdon[4] )
		draw4( &xlocs, &ylocs );
	    if( Lgdon[5] )
		draw5( &xlocs, &ylocs );
	}

	/* - If viewspace clipping is on: */
	else{

	    /* -- Determine location of data point relative to viewspace. */
	    iloc = INSIDE;
	    if( yloc > Yvs[2] )
		iloc = iloc + IABOVE;
	    else if( yloc < Yvs[1] )
		iloc = iloc + IBELOW;
	    if( xloc > Xvs[2] )
		iloc = iloc + IRIGHT;
	    else if( xloc < Xvs[1] )
		iloc = iloc + ILEFT;


	    /* -- There are three cases to handle:
	     *    (1) Entire line segment is inside viewspace:
	     *        Move has already been done. Do draw. */

	    if( (cmgdm.iold + iloc) == INSIDE ){
		if( Lgdon[1] )
		    draw1( &xlocs, &ylocs );
		if( Lgdon[2] )
		    draw2( xloc, yloc );
		if( Lgdon[3] )
		    draw3( &xlocs, &ylocs );
		if( Lgdon[4] )
		    draw4( &xlocs, &ylocs );
		if( Lgdon[5] )
		    draw5( &xlocs, &ylocs );
	    }

	    /*    (2) Part of line segment is inside viewspace:
	     *        Clip line segment to viewspace. Do move and draw. */
	    else if( ( cmgdm.iold & iloc ) == 0 ){
		Xline[1] = cmgdm.xold;
		Xline[2] = xloc;
		Yline[1] = cmgdm.yold;
		Yline[2] = yloc;
		Ixyloc[1] = cmgdm.iold;
		Ixyloc[2] = iloc;
		clipdp( xline, yline, ixyloc, cmgdm.xvs, cmgdm.yvs, &unused );

		if( Lgdon[1] ){
		    move1( &Xline[1], &Yline[1] );
		    draw1( &Xline[2], &Yline[2] );
		}
		if( Lgdon[2] ){
		    move2( Xline[1], Yline[1] );
		    draw2( Xline[2], Yline[2] );
		}
		if( Lgdon[3] ){
		    move3( &Xline[1], &Yline[1] );
		    draw3( &Xline[2], &Yline[2] );
		}
		if( Lgdon[4] ){
		    move4( &Xline[1], &Yline[1] );
		    draw4( &Xline[2], &Yline[2] );
		}
		if( Lgdon[5] ){
		    move5( &Xline[1], &Yline[1] );
		    draw5( &Xline[2], &Yline[2] );
		}
	    }

	    /*    (3) Entire line segment is outside viewspace: Do nothing. */

	    /* Save iloc information */
	    cmgdm.iold = iloc;
	}

	/* - Set current point to data point. */

	cmgdm.xold = xloc;
	cmgdm.yold = yloc;

L_8888:
	return;

} /* end of function */

