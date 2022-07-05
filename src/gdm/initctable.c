#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ initctable(name, name_s, nentry, nerr)
char *name;   int name_s;
int *nentry, *nerr;
{
	int j, j_, nbad;



	/*=====================================================================
	 * PURPOSE:  To initialize memory based color table from a named file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    name:    Name of color table to set. [c]
	 *             = 'default' for default color table.
	 *             = 'grayscale' for a gray-scale table.
	 *             = 'rainbow' for an interesting color table.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nentry:  Number of entries in the color table. [i]
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     MCTSIZE
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     nctsize, ctred, ctgreen, ctblue, ctname
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  readctable
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900803:  Original version taken from part of old setctablename.
	 *=====================================================================
	 * DOCUMENTED:  900803
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Read color table from disk file. */
	readctable( name,name_s, MCTSIZE, &cmgdm.ctred[0], &cmgdm.ctgreen[0], 
	 &cmgdm.ctblue[0], (char*)kmgdm.ctname[0],9, nentry, nerr );
	if( *nerr > 0 )
		goto L_8888;
	cmgdm.nctsize = *nentry - 1;

	/* - Range check input color table. */

	nbad = 0;
	for( j = 0; j <= cmgdm.nctsize; j++ ){
		j_ = j - 1;
		if( cmgdm.ctred[j] < 0.0 ){
			nbad = nbad + 1;
			cmgdm.ctred[j] = 0.0;
			}
		else if( cmgdm.ctred[j] > 1.0 ){
			nbad = nbad + 1;
			cmgdm.ctred[j] = 1.0;
			}
		if( cmgdm.ctgreen[j] < 0.0 ){
			nbad = nbad + 1;
			cmgdm.ctgreen[j] = 0.0;
			}
		else if( cmgdm.ctgreen[j] > 1.0 ){
			nbad = nbad + 1;
			cmgdm.ctgreen[j] = 1.0;
			}
		if( cmgdm.ctblue[j] < 0.0 ){
			nbad = nbad + 1;
			cmgdm.ctblue[j] = 0.0;
			}
		else if( cmgdm.ctblue[j] > 1.0 ){
			nbad = nbad + 1;
			cmgdm.ctblue[j] = 1.0;
			}
		}

	if( nbad > 0. ){
		*nerr = 2203;
		setmsg( "ERROR", *nerr );
		apimsg( nbad );
		aplmsg( "Values must be in the range 0.0 to 1.0",39 );
		aplmsg( "Name of color table is",23 );
		apcmsg( name,name_s );
		}

L_8888:
	return;

} /* end of function */

