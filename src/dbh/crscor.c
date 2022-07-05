#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*                                                             CRSCOR
 *  CRSCOR -- Subroutine to compute cross-correlation function
 *
 *  Author:  Dave Harris
 *           L-205
 *           Lawrence Livermore National Laboratory
 *           Livermore, Ca  94550
 *
 *  Input Arguments:
 *  ----- ----------
 *
 *    DATA1                  Array containing first sequence
 *
 *    DATA2                  Array containing second sequence
 *
 *    #SAMPLES               Number of samples in data
 *
 *    #WINDOWS               Requested number of windows
 *
 *    WINDOW_LENGTH          Requested number of samples in each window
 *                           the subroutine calculates window overlap.
 *                           Not to exceed 2048 in present configuration.
 *
 *    WINDOW_TYPE            Character*8 variable describing type of
 *                           data analysis window to be used.  Legal
 *                           types are:
 *
 *                                <HAM>MING
 *                                <HAN>NING
 *                                <C>OSINE
 *                                <R>ECTAN
 *                                <T>RIANG
 *
 *
 *  Output Arguments:
 *  ------ ----------
 *
 *    C                      Array containing resulting 2*WINDOW_LENGTH - 1
 *                           correlation coefficients.  The correlation
 *                           sequence is circularly rotated in the array,
 *                           rotated so that the zeroth lag is in C(0).
 *                           The array is dimensioned (0:4095).
 *
 *    FFT_LENGTH             Number of samples in correlation sequence.
 *                           Sequence may be padded with zeroes.
 *
 *    ERROR_MESSAGE          Error message, CHARACTER variable.
 *
 *
 *
 *  Linkage:  FFT, ZERO, COPY, WINDOW, RMS
 *
 *  Replacements
 *
 *
 *
 *  Author:  Dave Harris
 *
 *  Created:  January 30, 1980
 *
 *  Last Modified:  June 21, 1984
 * */
struct t_big {
/*	float caux[4095-(0)+1], w[2047-(0)+1], workr[4095-(0)+1], worki[4095-(0)+1]; */
	float *caux, *w, *workr, *worki;
	}	big;
void /*FUNCTION*/ crscor(data1, data2, nsamps, nwin, wlen, type, c, 
	 nfft, err, err_s)
float data1[], data2[];
int nsamps, nwin, wlen;
char *type;
float c[];
int *nfft;
char *err;   int err_s;
{
	char temp[131];
	int half, i, i_, j, j_, k, lsamp, nlags, nverlp, point;
	float scale, scale1, scale2, xi, xr, yi, yr;

	float *const Data1 = &data1[0] - 1;
	float *const Data2 = &data2[0] - 1;



	/*  Initializations
	 * */
	fstrncpy( err, err_s-1,  " ", 1 );


	/*  Check for legal window length and compute overlap
	 * */
	nlags = 2*wlen - 1;
	if( nwin < 1 ){

		fstrncpy( err, err_s-1, " CRSCOR - too few windows ", 26 );
		return;

		}
	else if( wlen < 1 || wlen > nsamps ){

		fstrncpy( err, err_s-1," CRSCOR - illegal window length " , 32 );
		return;

		}
	else{

		/*                                               Everything OK */

		if( nwin*wlen <= nsamps ){
			nverlp = 0;
			}
		else{
			nverlp = (nwin*wlen - nsamps);
			if (nwin > 1) {				
				nverlp = nverlp/(nwin - 1);
			}
			if( nwin*wlen - nverlp*(nwin - 1) > nsamps ){
				nverlp = nverlp + 1;
				}
			}
		lsamp = wlen - 1;

		}


	/*  Find first power of two >= #LAGS
	 * */
	*nfft = 8;
L_2:
	;
	if( *nfft >= nlags )
		goto L_3;
	*nfft = *nfft*2;
	goto L_2;
L_3:
	;
	half = *nfft/2;


        if ((big.w = (float *)malloc(wlen*sizeof(float))) == NULL) {
          printf("memory allocation failed in crscor\n");
          goto L_8892;
	}

        if ((big.caux = (float *)malloc(*nfft*sizeof(float))) == NULL) {
          printf("memory allocation failed in crscor\n");
          goto L_8891;
	}

        if ((big.workr = (float *)malloc(*nfft*sizeof(float))) == NULL) {
          printf("memory allocation failed in crscor\n");
          goto L_8890;
	}

        if ((big.worki = (float *)malloc(*nfft*sizeof(float))) == NULL) {
          printf("memory allocation failed in crscor\n");
          goto L_8889;
	}

	/*  Generate window
	 * */
	for( i = 0; i <= lsamp; i++ ){
		i_ = i - 1;
		big.w[i] = 1.;
		/*             I */
		}
	window( &big.w[0], wlen, type, 1, wlen, &big.w[0], err,err_s );

	/*  Check validity of window calculation
	 * */
	if( memcmp(err,"        ",8) != 0 ){
                fstrncpy(temp, 130, err, strlen(err));
                fstrncpy(temp+strlen(err),130-strlen(err), " (from CROSS)", 13);
                fstrncpy(err,err_s-1,temp,strlen(temp));
                goto L_8888;
		}

	/*  Compute cross-correlation function
	 *
	 *
	 *    Initialize window pointer
	 * */
	point = 1;

	/*    Initialize correlation arrays
	 * */
	zero( &c[0], *nfft );
	zero( &big.caux[0],*nfft );

	/*    Compute cross-spectrum for each window,  then average
	 * */
	for( i = 1; i <= nwin; i++ ){
		i_ = i - 1;

		/*    Zero work arrays
		 * */
		zero( &big.workr[0], *nfft );
		zero( &big.worki[0], *nfft );

		/*    Load data into arrays
		 * */
		copy( (int*)&Data1[point], (int*)&big.workr[0], wlen );
		copy( (int*)&Data2[point], (int*)&big.worki[0], wlen );

		/*    Compute scale factors
		 * */
		scale1 = rms( &big.workr[0], wlen );
		scale2 = rms( &big.worki[0], wlen );
		scale = scale1*scale2;

		/*    Window and scale data
		 * */
		for( j = 0; j <= lsamp; j++ ){
			j_ = j - 1;
			big.workr[j] = big.workr[j]*big.w[j]/scale1;
			big.worki[j] = big.worki[j]*big.w[j]/scale2;
			/*               J */
			}

		/*    Compute and average cross spectra
		 * */
		fft( &big.workr[0], &big.worki[0], *nfft, -1 );

		/*      Special case for point at 0
		 * */
		c[0] = c[0] + big.workr[0]*big.worki[0]*scale;

		/*      All other points
		 * */
		for( j = 1; j <= half; j++ ){
			j_ = j - 1;

			k = *nfft - j;

			xr = (big.workr[j] + big.workr[k])*.5;
			xi = (big.worki[j] - big.worki[k])*.5;
			yr = (big.worki[j] + big.worki[k])*.5;
			yi = (big.workr[k] - big.workr[j])*.5;

			c[j] = c[j] + (xr*yr + xi*yi)*scale;
			big.caux[j] = big.caux[j] + (xr*yi - xi*yr)*scale;
			c[k] = c[j];
			big.caux[k] = -big.caux[j];

			}

		/*    Update window pointer
		 * */
		point = point + wlen - nverlp;

		}

	/*    Inverse fft for correlation computation
	 * */
	fft( &c[0], &big.caux[0], *nfft, 1 );

	/*  Bye
	 * */

L_8888:
        free(big.worki);

L_8889:
        free(big.workr);

L_8890:
        free(big.caux);

L_8891:
        free(big.w);

L_8892:
	return;
} /* end of function */











