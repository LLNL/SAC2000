#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ detnum(kfmt, kfmt_s, nentry)
char *kfmt;   int kfmt_s;
int *nentry;
{
	char _c0[2];
	int imult, int_, iten, jc, ji, nc, nerr;

	/*=====================================================================
	 * PURPOSE:  To determine the number of output entries that would
	 *           result from reading a card using a given format statement.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KFMT:     Format statement to use in conversion. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NENTRY:   Number of floating point fields to be read. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  CNVATI
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *    970129:  Add parameter (0) to cnvati.  0 means that if a string
         *             of digits is too int, let it slide by.  maf 
	 *    860910:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  860910
	 *===================================================================== */
	/* - Initialize output argument. */
	*nentry = 0;

	/* - Determine number of characters in format statement. */

	nc = indexb( kfmt,kfmt_s );

	/* - Loop on each character in format statement.
	 *   Ignore the first and last characters which should be parentheses. */
	jc = 2;
	nc = nc - 1;

L_1000:
	if( jc <= nc ){

		/* -- Search for "F", "E", or "G". */
		if( (kfmt[jc - 1] == 'F' || kfmt[jc - 1] == 'E') || kfmt[jc - 
		 1] == 'G' ){
			/* --- If found, search backward to see if there is a preceeding multiplier. */
			imult = 0;
			iten = 1;
			ji = jc - 1;
L_2000:
			cnvati( &kfmt[ji - 1],1, &int_, 0, &nerr );
						/* add 0, maf 970129 */
			if( nerr == 0 ){
				imult = imult + iten*int_;
				iten = 10*iten;
				ji = ji - 1;
				goto L_2000;
				}
			/* --- Accumulate the number of entries here. */
			if( imult > 0 ){
				*nentry = *nentry + imult;
				}
			else{
				*nentry = *nentry + 1;
				}
			}

		/* -- Loop back until format statement is exhausted. */
		jc = jc + 1;
		goto L_1000;
		}

L_8888:
	return;

} /* end of function */

