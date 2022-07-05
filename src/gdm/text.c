#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ text(ktext, ktext_s, nctext)
char *ktext;   int ktext_s;
int nctext;
{
	int lrotated = FALSE; 
	int ltemp = FALSE;
	float _f0 = 0.0, _f1 = 0.0, del = 0.0, shgt = 0.0, swidth = 0.0, theta = 0.0, xchar = 0.0, xdel = 0.0, ychar = 0.0, ydel = 0.0;


	/*=====================================================================
	 * PURPOSE:  To display a text string at current plot location.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ktext:   Text string. [c]
	 *    nctext:  Number of characters in ktext to display. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    VSMALL
	 *    gdm:     ltsoft, twidth, thgt, tangle, ihjust, ivjust, xold, yold
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getstringsize, settextfont, softwaretext, 
	 *             text1, text2, text3, text4, move
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    swidth:  Width in plot coordinates of text string. [f]
	 *    shgt:    Height of text string. [f]
	 *    [xy]del: Offset due to current text justification attribute. [f]
	 *    del:     Total offset due to text justification. [f]
	 *    theta:   Rotation angle of offset. [f]
	 *    [xy]char: Position of lower left hand corner of first character. [f]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870416:  Deleted hardware text for graphics devices 3 and 4.
	 *    861017:  Major retstructuring.
	 *    850208:  Forced use of software text for rotated text requests.
	 *    831026:  Original Version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861017
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine string extent. */
	getstringsize( ktext, nctext, &swidth );
	shgt = cmgdm.thgt;

	/* - Determine relative move to make depending upon text justification.
	 *   (Left and bottom justification produces no relative move.) */

	xdel = 0.;
	if( cmgdm.ihjust == 2 ){
		xdel = -0.5*swidth;
		}
	else if( cmgdm.ihjust == 3 ){
		xdel = -swidth;
		}

	ydel = 0.;
	if( cmgdm.ivjust == 2 ){
		ydel = -0.5*shgt;
		}
	else if( cmgdm.ivjust == 3 ){
		ydel = -shgt;
		}

	/* - Apply rotation of offsets. */

	if( cmgdm.tangle != 0. && (xdel != 0. || ydel != 0) ){
		del = sqrt( powi(xdel,2) + powi(ydel,2) );
		theta = atan2( ydel, xdel );
		xdel = del*cos( theta + TORAD*cmgdm.tangle );
		ydel = del*sin( theta + TORAD*cmgdm.tangle );
		}

	/* - Plot text, making a relative move to account for text justification. */

	/* -- Software text is used when:
	 *    (1) software text has been requested
	 *    (2) hardware text requested but text is to be rotated.
         *        (unless writing sgf file (lgdon(2) = TRUE)         */

	lrotated = cmgdm.tangle < -VSMALL || cmgdm.tangle > VSMALL;
        if(cmgdm.ltsoft || lrotated ) ltemp = TRUE;

/* allow rotated hardware text if writing to SGF file */

        if( Lgdon[2] && (!cmgdm.ltsoft)) ltemp = FALSE;

	if( (ltemp) && (!cmgdm.lfhard) ){
		xchar = cmgdm.xold + xdel;
		ychar = cmgdm.yold + ydel;
		move( xchar, ychar );
		softwaretext( ktext, nctext );

		/* -- Unrotated hardware text.  Send to active graphics devices.
		 *    (Use software text for graphics devices 3 and 4.) */
		}
	else{
                if(!Lgdon[2]){ 
		  xchar = cmgdm.xold + xdel;
		  xchar = fmax( 0., fmin( 1., xchar ) );
		  ychar = cmgdm.yold + ydel;
		  ychar = fmax( 0., fmin( 1., ychar ) );
		  move( xchar, ychar );
	        }
		if( Lgdon[1] )
			hardwaretext1( ktext,ktext_s, &nctext );
		if( Lgdon[2] )
			hardwaretext2( ktext, nctext );
		if( Lgdon[3] )
			softwaretext( ktext, nctext );
		if( Lgdon[4] )
			softwaretext( ktext, nctext );
		if( Lgdon[5] )
			softwaretext( ktext, nctext );

		}

	/* - Move back to current position. */

	if (!Lgdon[2]) move( xchar - xdel, ychar - ydel );

        ltemp = cmgdm.ltsoft;

L_8888:
	return;

} /* end of function */

