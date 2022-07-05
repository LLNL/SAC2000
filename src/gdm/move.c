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

void /*FUNCTION*/ move(xloc, yloc)
float xloc, yloc;
{
	int iloc;
        float xloc1, yloc1;
	void move3(), move4(), move5();



	/*=====================================================================
	 * PURPOSE:  To move to the requested viewport point.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    xloc:    X viewport coordinate. [f]
	 *    yloc:    Y viewport coordinate. [f]
	 *             Range of xloc and yloc is 0. to 1.
	 *             Lower left hand corner is (0.,0.).
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
	 *   saclib:   move1, move2, move3, move4
	 *===================================================================== */
	/* PROCEDURE: */
        xloc1 = xloc;
        yloc1 = yloc;

	/* - If viewspace clipping is off, send move to active devices. */
	if( !cmgdm.lvsclip ){
	    if( Lgdon[1] )
		move1( &xloc1, &yloc1 );
	    if( Lgdon[2] )
		move2( xloc1, yloc1 );
	    if( Lgdon[3] )
		move3( &xloc1, &yloc1 );
	    if( Lgdon[4] )
		move4( &xloc1, &yloc1 );
	    if( Lgdon[5] )
		move5( &xloc1, &yloc1 );
	}

	/* - If viewspace clipping is on: */
	else{
	    /* -- Determine location of data point relative to viewport. */
	    iloc = INSIDE;
	    if( yloc > Yvs[2] ){
		iloc = iloc + IABOVE;
	    }
	    else if( yloc < Yvs[1] ){
		iloc = iloc + IBELOW;
	    }
	    if( xloc > Xvs[2] ){
		iloc = iloc + IRIGHT;
	    }
	    else if( xloc < Xvs[1] ){
		iloc = iloc + ILEFT;
	    }

	    /* -- Move to location if inside viewport. */
	    if( iloc == INSIDE ){
		if( Lgdon[1] )
		    move1( &xloc1, &yloc1 );
		if( Lgdon[2] )
		    move2( xloc1, yloc1 );
		if( Lgdon[3] )
		    move3( &xloc1, &yloc1 );
		if( Lgdon[4] )
		    move4( &xloc1, &yloc1 );
		if( Lgdon[5] )
		    move5( &xloc1, &yloc1 );
	    }

	    cmgdm.iold = iloc;
	}

	/* - Save current point. */

	cmgdm.xold = xloc;
	cmgdm.yold = yloc;

L_8888:
	return;

} /* end of function */

