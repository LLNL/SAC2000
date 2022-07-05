#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*                                                                   MEM
 *  Maximum entropy estimate by autocorrelation method.  Implements
 *  Yule-Walker method.
 *
 *
 *  Input Arguments:
 *  ----- ----------
 *
 *
 *    R              Vector of correlation coefficients.
 *
 *    N              Number of lags used by MEM algorithm.
 *
 *    FFT_SIZE       Size of fft for display.
 *
 *  Output Arguments:
 *  ------ ----------
 *
 *    SPECTRUM       REAL*4 array of length FFT_SIZE in which the spectrum
 *                   is returned.
 *
 *    ERROR_MESSAGE  CHARACTER*130 variable containing error message if an
 *                   an error occurs, ' ' otherwise.
 *
 *  Auxiliary Arguments:
 *  --------- ----------
 *
 *    AUXILIARY      REAL*4 scratch array of length FFT_SIZE.
 *
 *  Author:  David Harris
 *
 *  Last Modified:  December 28, 1984
 *
 *  Linkage:  FFT, ZERO, LEVIN
 *
 * */
void /*FUNCTION*/ mem(r, n, nfft, spect, errmsg, errmsg_s, aux)
float r[];
int n, nfft;
float spect[];
char *errmsg;   int errmsg_s;
float aux[];
{
	int i;
        float scale;
        float *a, *reflct;
        float *A, *Reflct;

	float *const Aux = &aux[0] - 1;
	float *const R = &r[0] - 1;
	float *const Spect = &spect[0] - 1;

        if((a = (float *)malloc(2*n*sizeof(float))) == NULL){
            strcpy(errmsg, "error allocating memory--mem\n");
            return;
	}

        if((reflct = (float *)malloc(2*n*sizeof(float))) == NULL){
            strcpy(errmsg, "error allocating memory--mem\n");
            free(a);
            return;
	}

        A = a-1;
        Reflct = reflct-1;

/*
	if( n > 100 ){
		fstrncpy( errmsg, errmsg_s-1, "MEM *** Maximum order (100) exceeded ***"
		 , 40 );
		return;
		}
*/
	/*  Zero arrays
	 * */
	zero( spect, nfft );
	zero( aux, nfft );

	/*  Invoke Levinson's recursion to compute prediction filter
	 * */
	levin( r, a, reflct, n );

	/*  Compute transfer function of prediction filter
	 * */
	for( i = 1; i <= n; i++ )
	    Spect[i] = A[i];

	fft( spect, aux, nfft, -1 );

	/*  Spectral estimate is reciprocal of filter's power spectrum
	 *
	 *    Scale factor is equal to prediction error
	 * */
	scale = 0.;
	for( i = 1; i <= n; i++ )
	    scale = scale + R[i]*A[i];

	for( i = 1; i <= nfft; i++ )
	    Spect[i] = scale/(powi(Spect[i],2) + powi(Aux[i],2));

	/*  Bye
	 * */

        free(a); free(reflct);

	return;
} /* end of function */

