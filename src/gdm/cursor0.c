#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ cursor0(xloc, yloc, kchar)
float *xloc, *yloc;
byte *kchar;
{
	void cursor3(), cursor4(), move3(), move4();
        void cursor5(), move5();

	/*=====================================================================
	 * PURPOSE:  To perform "locator" graphics input function.
	 *           The cursor is turned on and initially placed at xloc
	 *           and yloc.  When a single character is typed at the
	 *           terminal, the new cursor location and character are returned.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    xloc:    Initial X plot coordinate for cursor. [f]
	 *    yloc:    Initial Y plot coordinate for cursor. [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    xloc:    Current X plot coordinate for cursor. [f]
	 *    yloc:    Current Y plot coordinate for cursor. [f]
	 *    kchar:   Alphanumeric character typed at terminal. [c1]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     igdgin
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *   saclib:   move1, move2, move3, move4, 
	 *             cursor1, cursor2, cursor3, cursor4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    831026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870427
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Get character and cursor position from appropriate device. */
	if( Lgdon[1] ){
		move1( xloc, yloc );
		cursor1( xloc, yloc, kchar );
		}
	else if( Lgdon[2] ){
		move2( *xloc, *yloc );
		cursor2( *xloc, *yloc, *kchar );
		}
	else if( Lgdon[3] ){
		move3( xloc, yloc );
		cursor3( xloc, yloc, kchar, 1 );
		}
	else if( Lgdon[4] ){
		move4( xloc, yloc );
		cursor4( xloc, yloc, kchar, 1 );
		}
	else if( Lgdon[5] ){
		move5( xloc, yloc );
		cursor5( xloc, yloc, kchar, 1 );
		}

L_8888:
	return;

} /* end of function */

