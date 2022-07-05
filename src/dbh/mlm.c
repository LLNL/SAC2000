#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*                                                                   MLM
 *  Subroutine to compute maximum likelihood spectral estimate
 *
 *  Input Arguments:
 *  ----- ----------
 *
 *    R             REAL*4 array containing autocorrelation coefficients.
 *
 *    N             Number of autocorrelation lags (includes zero lag)
 *                  used in computation.  Integer variable.
 *
 *    FFT_SIZE      Size of fft used for calculating displayed spectrum.
 *                  Integer variable.
 *
 *  Output Arguments:
 *  ------ ----------
 *
 *    SPECTRUM      REAL*4 array of length FFT_SIZE containing spectrum.
 *
 *    ERROR_MESSAGE CHARACTER*130 variable containing error message if an
 *                  error is detected, ' ' otherwise.
 *
 *  Auxiliary Arguments:
 *  --------- ----------
 *
 *    AUXILIARY     REAL*4 scratch array of length FFT_SIZE
 *
 *
 *  Author:  David Harris
 *
 *  Last Modified:  December 28, 1984
 *
 *  Linkage:  ZERO, UNIT, COPY , STEP, FFT
 *
 * */
void /*FUNCTION*/ mlm(r, n, nfft, spect, errmsg, errmsg_s, aux)
float r[];
int n, nfft;
float spect[];
char *errmsg;   int errmsg_s;
float aux[];
{
	int i, j, k, n2;
        float fn, pi;
        float *a, *b, *c, *x;

        float *A, *B, *C, *X;

	float *const Aux = &aux[0] - 1;
	float *const R = &r[0] - 1;
	float *const Spect = &spect[0] - 1;


        if((a = (float *)malloc(n*sizeof(float))) == NULL){
	    strcpy(errmsg,"error allocating memory--mlm\n");
	    return;
	}
        if((b = (float *)malloc(n*sizeof(float))) == NULL){
	    strcpy(errmsg,"error allocating memory--mlm\n");
	    free(a);
	    return;
	}

        if((c = (float *)malloc(n*sizeof(float))) == NULL){
	    strcpy(errmsg,"error allocating memory--mlm\n");
	    free(a); free(b);
	    return;
	}

        if((x = (float *)malloc(n*sizeof(float))) == NULL){
	    strcpy(errmsg,"error allocating memory--mlm\n");
	    free(a); free(b); free(c);
	    return;
	}

        A = a-1;
        B = b-1;
        C = c-1;
        X = x-1;

	pi = 3.14159265;
/*
	if( n > 100 ){
		fstrncpy( errmsg, errmsg_s-1, "MLM *** Maximum order (100) exceeded ***"
		 , 40 );
		return;
		}
*/
	/*  Invert autocorrelation matrix using Simpson's sideways recursion
	 *  and set up quadratic form for spectral estimate */
	zero( x, n );

	unit( r, a, n );
	copy( (int*)a, (int*)c, n );

	n2 = n/2;
	for( i = 1; i <= n2; i++ ){
	    if( i != 1 )
		step( r, a, b, c, n );


	    for( j = i; j <= n; j++ ){
		k = n - j + i;
		X[k] = X[k] + C[j];
	    }

	    for( j = 1; j <= i; j++ ){
		k = n - i + j;
		X[k] = X[k] + C[j];
	    }

	    copy( (int*)c, (int*)b, n );

	}

	if( 2*n2 < n ){

	    step( r, a, b, c, n );
	    i = n2 + 1;
	    for( j = i; j <= n; j++ ){
		k = n - j + i;
		X[k] = X[k] + C[j];
	    }
	}

	/*  Evaluate quadratic form using fft */
	zero( spect, nfft );
	Spect[1] = X[n];
	for( i = 2; i <= n; i++ ){
	    Spect[i] = X[n + 1 - i];
	    Spect[nfft - n + i] = X[i - 1];
	}

	zero( aux, nfft );
	fft( spect, aux, nfft, -1 );

	/*  Invert quadratic form and scale
	 * */
	fn = (float)( n );
	for( i = 1; i <= nfft; i++ )
	    Spect[i] = fn/Spect[i];

	/*  Bye */

        free(a); free(b); free(c); free(x);

	return;
} /* end of function */

