#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ cursortext(xloc, yloc, ktext, ktext_s)
float *xloc, *yloc;
char *ktext;   int ktext_s;
{
	void cursortext3(), cursortext4(), move3(), move4();
        void cursortext5(), move5();


	/*=====================================================================
	 * PURPOSE:  To get cursor location and text from the "graphics window."
	 *           The cursor is turned on and initially placed at XLOC
	 *           and YLOC.  When a line of text terminated by a carriage-
	 *           return is typed in the graphics window, the new cursor 
	 *           location and the text are returned.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    xloc:    Initial X plot coordinate for cursortext. [f]
	 *    yloc:    Initial Y plot coordinate for cursortext. [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    xloc:    Current X plot coordinate for cursortext. [f]
	 *    yloc:    Current Y plot coordinate for cursortext. [f]
	 *    ktext:   Text typed (terminated by typing a carriage-return). [c]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     igdgin
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *   saclib:   move1, move2, move3, move4, 
	 *             cursortext1, cursortext2, cursortext3, cursortext4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870323:  Original version based upon cursor.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870427
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Get cursor position and text entered from appropriate device. */
	if( Lgdon[1] ){
		move1( xloc, yloc );
		cursortext1( *xloc, *yloc, ktext,ktext_s );
		}
	else if( Lgdon[2] ){
		move2( *xloc, *yloc );
		cursortext2( *xloc, *yloc, ktext );
		}
	else if( Lgdon[3] ){
		move3( xloc, yloc );
		cursortext3( xloc, yloc, ktext,ktext_s );
		}
	else if( Lgdon[4] ){
		move4( xloc, yloc );
		cursortext4( xloc, yloc, ktext,ktext_s );
		}
	else if( Lgdon[5] ){
		move5( xloc, yloc );
		cursortext5( xloc, yloc, ktext,ktext_s );
		}

L_8888:
	return;

} /* end of function */

