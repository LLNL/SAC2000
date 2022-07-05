#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*                                                               CRIT
 *  Criterion for picking mem order.
 *  Produces graph of normalized rms prediction error as a
 *  function of predictor order.
 *
 *  Input Arguments:
 *  ----- ----------
 *
 *    R            Vector of correlation coefficients
 *
 *    N            Number of correlation coefficients
 *
 *  Output Arguments:
 *  ------ ----------
 *
 *    X            Real array - first min(N, 100) points contain
 *                 rms prediction error.
 *
 *  Author:  David Harris
 *
 *  Last Modified:  August 7, 1980
 * */
void /*FUNCTION*/ crit(r, n, x)
float r[];
int n;
float x[];
{
	int i, i_, nr;
	float a[100], reflct[100];

	float *const A = &a[0] - 1;
	float *const R = &r[0] - 1;
	float *const Reflct = &reflct[0] - 1;
	float *const X = &x[0] - 1;




	zero( x, 100 );
	if( n > 100 ){
		nr = 100;
		}
	else{
		nr = n;
		}

	/*  Levinson's recursion to obtain reflection coefficients
	 * */
	levin( r, a, reflct, nr );

	/*  Recursion to compute normalized error
	 * */
	X[1] = 1.;
	for( i = 2; i <= nr; i++ ){
		i_ = i - 1;
		X[i] = X[i - 1]*sqrt( 1. - powi(Reflct[i - 1],2) );
		/*             I */
		}

	return;
} /* end of function */

