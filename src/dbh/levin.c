#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
/*                                                            LEVIN
 *   Subroutine to solve Durbin's problem - Toeplitz
 *   normal equations, right hand vector of autocorrelations
 *   In this version, the maximum problem size is 100.
 *
 *
 *  Author:  David Harris
 *
 *  Last Modified:  	July 23, 1996.	added casting operations to 
 *					improve rounding errors
 *			October 10, 1981
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
float r[], a[], reflct[];
int n;
{
	int idx, im1, jdx;
	double rhoDenom, rhoNum, rho;
        float *temp;
        float *Temp;

	float *const A = &a[0] - 1;
	float *const R = &r[0] - 1;
	float *const Reflct = &reflct[0] - 1;

        temp = (float *)malloc(n*sizeof(float));
        Temp = temp-1;

	/*  Initialize first two coefficients
	 * */
	A[1] = 1.;
	A[2] = - (double) ( R[2] ) / (double) ( R[1] ) ;
	Reflct[1] = A[2];

	/*  Using Levinson's recursion, determine the rest of the coefficients.
	 *  It is assumed that the filter is of length N, including the lead
	 *  coefficient which is always one.
	 * */
	if( n >= 3 ){
	    for( idx = 3; idx <= n; idx++ ){
		im1 = idx - 1;
		rhoNum = R[idx];
		rhoDenom = R[1];

		for( jdx = 2; jdx <= im1; jdx++ ){
		    rhoNum += (double) A[jdx] * (double) ( R[idx + 1 - jdx] ) ;
		    rhoDenom += (double) ( A[jdx] ) * (double) ( R[jdx] ) ;
		} /* end for(jdx) */

		rho = -rhoNum/rhoDenom;
		Reflct[im1] = rho;

		for( jdx = 2; jdx <= im1; jdx++ )
		    Temp[jdx] = (double) A[jdx] +
				rho * (double) ( A[idx + 1 - jdx] ) ;

		for( jdx = 2; jdx <= im1; jdx++ )
		    A[jdx] = Temp[jdx];

		A[idx] = rho;

	    } /* end for(idx) */
	} /* end if */

        free(temp);

	return;
} /* end of function */

