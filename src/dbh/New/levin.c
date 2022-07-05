#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*                                                            LEVIN
 *   Subroutine to solve Durbin's problem - Toeplitz
 *   normal equations, right hand vector of autocorrelations
 *   In this version, the maximum problem size is 100.
 *
 *
 *  Author:  David Harris
 *
 *  Last Modified:  October 10, 1981
 *
 *
 *  Input Arguments:
 *  ----------------
 *
 *    R                            Vector of autocorrelations
 *
 *    N                            Number of autocorrelations
 *                                 also the filter length (includes
 *                                 the first coefficient, which is
 *                                 always one).  May not exceed 100.
 *
 *  Output Arguments:
 *  -----------------
 *
 *    A                            Vector of filter coefficients
 *
 *    REFLCT                       Vector of reflection coefficients
 *                                 there are n-1 of these
 *
 *
 *  Linkage: (none)
 *
 * */
void /*FUNCTION*/ levin(r, a, reflct, n)
double r[], a[], reflct[];
int n;
{
	int i, i_, im1, j, j_;

	double d, e, rho;
        double *temp;
        double *Temp;

	double *const A = &a[0] - 1;
	double *const R = &r[0] - 1;
	double *const Reflct = &reflct[0] - 1;

        temp = (double *)malloc(n*sizeof(double));
        Temp = temp-1;

	/*  Initialize first two coefficients
	 * */
	A[1] = 1.;
	A[2] = -R[2]/ R[1];
	Reflct[1] = A[2];

	/*  Using Levinson's recursion, determine the rest of the coefficients.
	 *  It is assumed that the filter is of length N, including the lead
	 *  coefficient which is always one.
	 * */
	if( n >= 3 ){

		for( i = 3; i <= n; i++ ){
			i_ = i - 1;

			im1 = i - 1;
			e = R[i];
			d = R[1];

			for( j = 2; j <= im1; j++ ){
				j_ = j - 1;
				e = e + A[j]* R[i + 1 - j];
				d = d + A[j]* R[j];
				}

			rho = -e/d;
			Reflct[im1] = rho;

			for( j = 2; j <= im1; j++ ){
				j_ = j - 1;
				Temp[j] = A[j] + rho * A[i + 1 - j];
				}

			for( j = 2; j <= im1; j++ ){
				j_ = j - 1;
				A[j] = Temp[j];
				}

			A[i] = rho;

			}
		}

        free(temp);

	return;
} /* end of function */

