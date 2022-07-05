#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ sorti(iain, niain, lincr, iaout)
int iain[], niain;
int lincr;
int iaout[];
{
	int lagain;
	int i, j, j1, j1_, j2, j_, nd2;

	int *const Iain = &iain[0] - 1;
	int *const Iaout = &iaout[0] - 1;


	/* Ind
	 *=====================================================================
	 * PURPOSE:
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:
	 *=====================================================================
	 * GLOBAL INPUT:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * ASSUMPTIONS:
	 *=====================================================================
	 * LIMITATIONS:
	 *=====================================================================
	 * KNOWN ERRORS:
	 *=====================================================================
	 * EXTERNAL DEPENDENCIES:
	 *===================================================================== */
	/* SIMPLE BUBBLE SORT OF INTEGER ARRAY
	 *    - EITHER IN PLACE OR INTO ANOTHER ARRAY
	 *    - IN EITHER INCREASING OR DECREASING ORDER */
	/*      IAIN:     INTEGER INPUT (UNSORTED) ARRAY
	 *     NIAIN:     NUMBER OF ELEMENTS IN IAIN
	 *     LINCR:     LOGICAL SORT FLAG
	 *              IF .TRUE. - SORT IN INCREASING ORDER
	 *              IF .FALSE. - SORT IN DECREASING ORDER
	 *     IAOUT:=    INTEGER OUTPUT (SORTED) ARRAY
	 *             _MAY BE SAME ARRAY AS INPUT ARRAY? */
	/* PROCEDURE: */
	/* COPY INPUT ARRAY TO OUTPUT ARRAY */
	for( j = 1; j <= niain; j++ ){
		j_ = j - 1;
		Iaout[j] = Iain[j];
		}

	/* SORT HERE IF NIAIN GT 1 */

	if( niain > 1 ){
L_2000:
		lagain = FALSE;
		for( j = 1; j <= (niain - 1); j++ ){
			j_ = j - 1;
			if( Iaout[j] > Iaout[j + 1] ){
				i = Iaout[j];
				Iaout[j] = Iaout[j + 1];
				Iaout[j + 1] = i;
				lagain = TRUE;
				}
			}
		if( lagain )
			goto L_2000;

		/* REVERSE ORDER OF OUTPUT ARRAY IF REQUESTED */

		if( !lincr ){
			nd2 = niain/2;
			j2 = niain;
			for( j1 = 1; j1 <= nd2; j1++ ){
				j1_ = j1 - 1;
				i = Iaout[j1];
				Iaout[j1] = Iaout[j2];
				Iaout[j2] = i;
				j2 = j2 - 1;
				}
			}
		}

	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    810000:  Original version.
	 *===================================================================== */

} /* end of function */

