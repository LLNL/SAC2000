#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/datafilelist.h"
int /*FUNCTION*/ nextinputfile(ientry)
int *ientry;
{
	int nextinputfile_v;



	/*=====================================================================
	 * PURPOSE:  To get the next entry in the input "data file list" (dfl).
	 *=====================================================================
	 * ARGUMENTS:
	 *    ientry  =:=  Entry number. [i]
	 *                 To initialize, set to 0 before the first call.
	 *                 DO NOT CHANGE THIS VALUE BETWEEN CALLS.
	 *=====================================================================
	 * FUNCTION VALUE: .TRUE. if there are more entries in dfl.
	 *                 .FALSE. if there are no more entries in dfl.
	 *=====================================================================
	 * MODULE/LEVEL:  datafilelist/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    datafilelist:  kselectmode, iselect, nselect
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    datafilelist:  nentries, jselect
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900409:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900409
	 *===================================================================== */
	/* PROCEDURE: */
	/* - If in "ALL" mode: */
	if( strcmp(kmdatafilelist.kselectmode,"ALL     ") == 0 ){

		/* -- Initialize entry pointer and number of files if necessary. */
		if( *ientry <= 0 ){
			*ientry = 0;
			getnfiles( &cmdatafilelist.nentries );
			}

		/* -- Increment entry number if there are more entries in dfl. */
		if( *ientry < cmdatafilelist.nentries ){
			*ientry = *ientry + 1;
			nextinputfile_v = TRUE;

			/* -- Zero entry number and return with FALSE value if no more entries. */
			}
		else{
			*ientry = 0;
			nextinputfile_v = FALSE;
			}

		/* - If in "SELECT" mode: */

		}
	else{

		/* -- Initialize select pointer and if necessary. */
		if( *ientry <= 0 )
			cmdatafilelist.jselect = 0;

		/* -- Increment select pointer if there are more selections.
		 *    Set entry number to selected entry. */
		if( cmdatafilelist.jselect < cmdatafilelist.nselect ){
			cmdatafilelist.jselect = cmdatafilelist.jselect + 1;
			*ientry = Iselect[cmdatafilelist.jselect];
			nextinputfile_v = TRUE;

			/* -- Zero entry number and return with FALSE value if no more selections. */
			}
		else{
			*ientry = 0;
			nextinputfile_v = FALSE;
			}

		}

L_8888:
	return( nextinputfile_v );

} /* end of function */

