#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/datafilelist.h"
void /*FUNCTION*/ selectinputfiles(list, nlist)
int list[], nlist;
{
	int j, j_;

	int *const List = &list[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To select the active entries in the input "data file list"
	 *           (dfl) to use in subsequent action commands.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    list:   List of active entries in input dfl. [ia]
	 *            Numbers in this list refer to order of the entries in
	 *            the dfl. If entry is in this list it is active for
	 *            subsequent commands. If not in list, it is inactive.
	 *    nlist:  Length of list. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  datafilelist/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    datafilelist:  iselect, nselect
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900409:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900409
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Save list in common block. */
	cmdatafilelist.nselect = min( nlist, MDFL );

	for( j = 1; j <= nlist; j++ ){
		j_ = j - 1;
		Iselect[j] = List[j];
		}

L_8888:
	return;

} /* end of function */

