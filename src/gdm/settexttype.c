#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ settexttype(kqual)
char *kqual;
{



	/*=====================================================================
	 * PURPOSE:  To change the graphics text quality.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kqual:   Quality of text desired. [c]
	 *             = 'SOFTWARE' for software quality text.
	 *             = 'HARDWARE' for hardware quality text.
	 *             (Only the first character need be entered.)
	 *=====================================================================
	 * SPECIAL NOTE:  Some of the devices simulate hardware text by using
	 * the simplest software text font (font 1.)   Therefore, if you select
	 * software text after first selecting hardware text you may also
	 * have reselect the desired software font by calling settextfont.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     ltsoft
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861017:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861017
	 *===================================================================== */
	/* PROCEDURE: */
	/* - If first character is an "H", turn software quality text flag off.
	 *   Otherwise, turn software quality text flag on. */
	if( kqual[0] == 'H' || kqual[0] == 'h' ){
		cmgdm.ltsoft = FALSE;
		}
	else{
		cmgdm.ltsoft = TRUE;
		}

	/* - If hardware text has been chosen, select the simplest font. */

	if( !cmgdm.ltsoft )
		settextfont( 1 );

L_8888:
	return;

} /* end of function */

