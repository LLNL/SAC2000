#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/datafilelist.h"
void /*FUNCTION*/ setinputmode(mode)
char *mode;
{
	byte test;


	/*=====================================================================
	 * PURPOSE:  To set the input "data file list" (dfl) mode to use in
	 *           subsequent action commands.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    mode:  Input dfl mode. [c]
	 *           = 'ALL' to activate all entries in dfl. [default]
	 *           = 'SELECT' to activate selected entries in dfl.
	 *           Use "select_dfl_entries" to select specific entries.
	 *           Case insensitive. Only first character is required.
	 *=====================================================================
	 * MODULE/LEVEL:  datafilelist/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    datafilelist:  kselectmode
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:   modcase
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900409:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900409
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Convert first input character to upper case. */
	modcase( TRUE, mode, 1, &test );

	/* - Test versus allowed options. */

	if( test == 'A' ){
		strcpy( kmdatafilelist.kselectmode, "ALL     " );
		}
	else if( test == 'S' ){
		strcpy( kmdatafilelist.kselectmode, "SELECT  " );
		}
	else{
		strcpy( kmdatafilelist.kselectmode, "ALL     " );
		}

L_8888:
	return;

} /* end of function */

