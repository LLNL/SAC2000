#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*                                                                      UNIT
 *  Subroutine to calculate first column of inverse toeplitz matrix
 *
 *        Solves:   R a  =  (1,0,...,0)'
 *        where R is a Toeplitz matrix specified by the correlation
 *        coefficients R(1),...,R(N)
 *
 *        Author:  David Harris
 *
 *  Last Modified:  December 28, 1984
 * */
void /*FUNCTION*/ unit(r, a, n)
float r[], a[];
int n;
{
	int i, i_, j, j_, k, k_, nm1;
/*	float x[100];
*/
        float *x, *X;

	double alpha, beta, s;

	float *const A = &a[0] - 1;
	float *const R = &r[0] - 1;


        x = (float *)malloc(n*sizeof(float));
        X = x-1;

	zero( a, n );
        zero( x, n);

	/*        First step of recursion
	 * */
	A[1] = 1./R[1];

	/*        Remaining steps of recursion
	 * */
	nm1 = n - 1;
	for( i = 1; i <= nm1; i++ ){
		i_ = i - 1;
		s = 0;
		for( k = 1; k <= i; k++ ){
			k_ = k - 1;
			s = s + R[i + 2 - k]*A[k];
			}
		beta = 1./(s - 1./s);
		alpha = -beta/s;
		X[1] = alpha*A[1];
		X[i + 1] = beta*A[1];
		if( i > 1 ){
			for( k = 2; k <= i; k++ ){
				k_ = k - 1;
				X[k] = alpha*A[k] + beta*A[i + 2 - k];
				}
			}
		for( j = 1; j <= n; j++ ){
			j_ = j - 1;
			A[j] = X[j];
			}
		}

        free(x);

	return;
} /* end of function */

