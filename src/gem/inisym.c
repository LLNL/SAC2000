#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ inisym(iisym, nisym)
int iisym[], *nisym;
{
	int j, j_;

	int *const Iisym = &iisym[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To initialize the "standard" symbol list.
	 *           This is the list of symbols used in plotting data.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    IISYM:   List of "standard" pens.
	 *    NISYM:   Length of IISYM.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    830115:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861112
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Set up standard symbol table. */
	*nisym = 15;
	for( j = 1; j <= *nisym; j++ ){
		j_ = j - 1;
		Iisym[j] = j + 1;
		}

L_8888:
	return;

} /* end of function */

