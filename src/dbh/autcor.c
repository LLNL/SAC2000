#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/spe.h"
/*                                                             AUTCOR
 *  AUTCOR -- Subroutine to compute auto-correlation function.
 *
 *  Author:  Dave Harris
 *           L-205
 *           Lawrence Livermore National Laboratory
 *           Livermore, Ca  94550
 *
 *  Input Arguments:
 *  ----- ----------
 *
 *    DATA                   Array containing data sequence
 *
 *    DELTA                  REAL*4 variable containing data sampling
 *                           interval in seconds.
 *
 *    #SAMPLES               Number of samples in data
 *
 *    #WINDOWS               Requested number of windows
 *
 *    WINDOW_LENGTH          Requested number of samples in each window
 *                           the subroutine calculates window overlap.
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
 *    SCALING                Character*10 variable indicating type of desired
 *                           scaling:  'S...' for stochastic process power
 *                           density scaling, 'T...' for transient energy
 *                           density scaling.
 *
 *
 *  Output Arguments:
 *  ------ ----------
 *
 *    CORRELATIONS           Array containing resulting 2*WINDOW_LENGTH - 1
 *                           correlation coefficients.  The correlation
 *                           sequence is circularly rotated in the array,
 *                           rotated so that the zeroth lag is in R(1).
 *
 *    C_SIZE                 Padded size (padded with zeroes) of correlation
 *                           sequence.  A power of two, since FFTs are used
 *                           to perform correlations.
 *
 *    #LAGS                  Number of non-zero correlation function samples.
 *
 *    ERROR_MESSAGE          Error message, CHARACTER*130 variable.
 *
 *  Auxiliary Arguments:
 *  --------------------
 *
 *    AUXILIARY              A REAL*4 array of length at least 10,240
 *                           in present configuration.  Five times the
 *                           maximum window length.
 *
 *
 *  Linkage:  FFT, ZERO, WINDOW, WINMOV
 *
 *  Replacements
 *
 *
 *
 *  Author:  Dave Harris
 *
 *  Created:  December 26, 1984   Adapted from CRSCOR
 *
 *  Last Modified:  January 3, 1985
 *                  May 28, 1998, allow variable window length. maf
 * */
void /*FUNCTION*/ autcor(data, delta, nsamps, nwin, wlen, type, stype, 
	 ac, nfft, nlags, err, err_s, aux, ridge_fac)
float data[];
double delta;
int nsamps, nwin, wlen;
char *type, *stype;
float ac[];
int *nfft, *nlags;
char *err;   int err_s;
float aux[];
float ridge_fac;
{
	char temp[131];
	int half, i, iptr, j, k, lsamp, mxfft, nverlp, point, 
	 rptr, wcntr, wptr;
	float scale, xi, xr, yi, yr;

	float *const Ac = &ac[0] - 1;
	float *const Aux = &aux[0] - 1;
	float *const Data = &data[0] - 1;



	mxfft = cmspe.firstPowerOf2 * 2;
	wptr = 1;
	rptr = 1 + wlen;
	iptr = rptr + mxfft;

	/*  Check for legal window length and compute overlap
	 * */
	*nlags = 2*wlen - 1;
	if( nwin < 1 ){
		fstrncpy( err,err_s-1, "AUTCOR  *** Too few windows ***",31);
		return;
	}
	else if( wlen < 1 || wlen > nsamps ){
		fstrncpy( err,err_s-1,"AUTCOR *** Illegal window length ***" ,36);
		return;
	}
	else{
		/* Everything OK */
		fstrncpy( err , err_s-1 , " " , 1 );

		if( nwin*wlen <= nsamps )
			nverlp = 0;
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

	/*  Generate window
	 * */
	for( i = 0; i <= lsamp; i++ ){
		Aux[wptr + i] = 1.;
	}
	window( &Aux[wptr], wlen, type, 1, wlen, &Aux[wptr], err ,err_s );

	/*      Normalize window
	 * */
	scale = 0.;
	for( i = 0; i <= lsamp; i++ ){
		scale = scale + powi(Aux[wptr + i],2);
	}


	if( stype[0] == 'S' ){

		scale = sqrt( delta/((float)( nwin )*scale) );

	}
	else if( stype[0] == 'T' ){

		scale = sqrt( delta*(float)( wlen )/scale );

	}

	for( i = 0; i <= lsamp; i++ ){
		Aux[wptr + i] = Aux[wptr + i]*scale;
	}

	/*  Check validity of window calculation
	 * */
	if( memcmp(err,"        ",8) != 0 ){
                fstrncpy(temp , 130 , err , strlen(err));
                fstrncpy(temp+strlen(err),130-strlen(err)," (from AUTCOR)", 14);
		fstrncpy( err, err_s-1 , temp, strlen(temp) );
		return;
	}

	/*  Find first power of two >= #LAGS
	 * */
	*nfft = 8;

	while ( *nfft < *nlags )
		( *nfft ) *= 2 ;

	half = *nfft/2;

	/*  Compute autocorrelation function
	 *
	 *
	 *    Initialize window pointer
	 * */
	point = 1;

	/*    Initialize correlation array and window counter
	 * */
	zero( ac, *nfft );
	wcntr = 0;

	/*    Compute autospectrum for each window,  then average
	 * */

	while ( wcntr < nwin ) {

	    /*    Window and load data into arrays - do two windows at a time.
	     * */
	    int idx ;	/* index */
	    zero( &Aux[rptr], *nfft );
	    winmov( &Data[point], wlen, &Aux[wptr], &Aux[rptr] );
	    point = point + wlen - nverlp;
	    wcntr = wcntr + 1;

	    zero( &Aux[iptr], *nfft );
	    if( wcntr < nwin ){
		winmov( &Data[point], wlen, &Aux[wptr], &Aux[iptr] );
		point = point + wlen - nverlp;
		wcntr = wcntr + 1;
	    }

	    /*    Compute and average autospectra
	     * */
	    fft( &Aux[rptr], &Aux[iptr], *nfft, -1 );

	    /*      Special case for point at 0
	     * */
	    Ac[1] = Ac[1] + powi(Aux[rptr],2) + powi(Aux[iptr],2);

	    /*      All other points
	     * */
	    for( j = 1; j <= half; j++ ){
		k = *nfft - j;

		xr = (Aux[rptr + j] + Aux[rptr + k])*.5;
		xi = (Aux[iptr + j] - Aux[iptr + k])*.5;
		yr = (Aux[iptr + j] + Aux[iptr + k])*.5;
		yi = (Aux[rptr + k] - Aux[rptr + j])*.5;

		Ac[1 + j] = Ac[1 + j] + (powi(xr,2) + powi(xi,2)) + (powi(yr,2) + 
		 powi(yi,2));
		Ac[1 + k] = Ac[1 + j];
	    }
	} /* end while ( wcntr < nwin ) */


	/*    Inverse fft for correlation computation
	 * */
	zero( &Aux[iptr], *nfft );
	fft( &Ac[1], &Aux[iptr], *nfft, 1 );

        /* apply ridge regression factor */
/*        ac[0] *= 1.00001;  */
        ac[0] *= (1.0 + ridge_fac);

     
	/*  Bye
	 * */

	return;
} /* end of function */

