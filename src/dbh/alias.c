#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*  ALIAS - Subroutine to alias a sequence for sparse transform
 *          calculations.
 *
 *  Author:  Dave Harris
 *
 *  Created:  August 3, 1981
 *
 *  Last Modified:
 *
 *
 *  Input Arguments:
 *  ----------------
 *
 *    INPUT               Real*4 array containing input sequence.
 *
 *    ORIGIN              Index of input sequence origin
 *
 *    INPUT_LENGTH        Length of input sequence.
 *
 *    ALIASED_LENGTH      Desired length of output sequence.
 *
 *
 *  Output Arguments:
 *  -----------------
 *
 *    ALIASED             Real*4 array containing aliased sequence.
 *
 *
 *  Linkage:  ZERO
 * */
void /*FUNCTION*/ alias(x, origin, m, n, y)
float x[];
int origin, m, n;
float y[];
{
	int iptr, optr;

	float *const X = &x[0] - 1;
	float *const Y = &y[0] - 1;




	zero( y, n );

	iptr = 1;

	if( origin == 1 ){

		optr = 1;

		}
	else if( origin > 1 ){

		optr = n + 1 - ((origin - 1) - (origin/n)*n);

		}

L_1:
	;
	if( iptr > m )
		goto L_2;

	Y[optr] = Y[optr] + X[iptr];

	iptr = iptr + 1;
	optr = optr + 1;

	/*                                                           Wrap-around */
	if( optr > n ){

		optr = optr - n;

		}

	goto L_1;
L_2:
	;

	/*  Done
	 * */
	return;
} /* end of function */

