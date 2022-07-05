#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*  WINDOW - Subroutine to window a sequence with various windows.
 *           The sequence is set to zero outside of the window.
 *
 *  Author:  Dave Harris
 *
 *  Created:  August 14, 1981
 *
 *  Last Modified:  August 26, 1991 - Changed nint(x) to int(x + .5) to improve
 *                                    portability to DEC 5000 (wct).
 *
 *  Input Arguments:
 *  ----------------
 *
 *    X               Real array containing the sequence to be
 *                    windowed.
 *
 *    N               The length of the sequence.
 *
 *    TYPE            Character*8 variable containing code for
 *                    the type of window.
 *                      'HAM.....'     Hamming window
 *                      'HAN.....'     Hanning window
 *                      'R.......'     Rectangular window
 *                      'C.......'     10 percent cosine taper window
 *                      'T.......'     Triangular window
 *
 *    FIRST_SAMPLE    Integer variable containing window first sample
 *                    index.
 *
 *    WINDOW_LENGTH   Integer variable containing window length in samples.
 *
 *  Output Arguments:
 *  -----------------
 *
 *    Y               Real array containing windowed sequence.
 *                    May be the same array as X for in-place
 *                    processing.
 *
 *    ERROR_CONDITION Character variable with error condition message,
 *                    if error occurs, blank otherwise.
 *
 *  Linkage:  ZERO
 *
 *  Replacements
 *
 * */
void /*FUNCTION*/ window(x, n, type, fsamp, wlen, y, err, err_s)
float x[];
int n;
char *type;
int fsamp, wlen;
float y[];
char *err;   int err_s;
{
	int i, ib, ie, ilen, j, lsamp;
	float center, extent, f0, f1, omega, pi;

	float *const X = &x[0] - 1;
	float *const Y = &y[0] - 1;




	/*  Error Checking
	 * */
	lsamp = fsamp + wlen - 1;

	if( fsamp < 1 || fsamp > n ){
		fstrncpy( err, err_s-1, " WINDOW - First index out of bounds ", 36 );
		return;
	}
	else if( lsamp < 1 || lsamp > n ){
		fstrncpy( err, err_s-1, " WINDOW - Window length out of bounds ", 38 );
		return;
	}
	else if( wlen <= 0 ){
		fstrncpy( err, err_s-1, " WINDOW - Illegal window length ", 32 );
		return;
	}
	else{
		fstrncpy( err, err_s-1, " ", 1 );
	}

	pi = 3.14159265;

	/*  Multiply the signal inside the window by the window function.
	 *
	 *  Rectangular window */
	if( type[0] == 'R' ){
		for( i = fsamp; i <= lsamp; i++ ){
			Y[i] = X[i];
		}
	}

	/*  Hamming or Hanning window */
	else if( type[0] == 'H' ){

		omega = 2.*pi/(float)( wlen - 1 );

		/*  Hamming window */
		if( memcmp(type,"HAM",3) == 0 ){
			f0 = .54;
			f1 = .46;
		}

		/*  Hanning window */
		else{
			f0 = .5;
			f1 = .5;
		}

		for( i = fsamp; i <= lsamp; i++ ){
			Y[i] = (f0 + f1*cos( omega*(i - fsamp) - pi ))*X[i];
		}
	}

	/* Cosine taper (10%) window */
	else if( type[0] == 'C' ){

		ilen = (int)( wlen/10. + .5 );
		if( ilen > 0 ){
			omega = pi/(float)( ilen );
		}
		ib = fsamp + ilen;
		for( i = fsamp; i <= (ib - 1); i++ ){
			j = i - fsamp;
			Y[i] = X[i]*0.5*(1. - cos( omega*(float)( j ) ));
		}
		ie = lsamp - ilen;
		for( i = ib; i <= ie; i++ ){
			Y[i] = X[i];
		}
		for( i = ie + 1; i <= lsamp; i++ ){
			j = lsamp - i;
			Y[i] = X[i]*0.5*(1. - cos( omega*(float)( j ) ));
		}
	}

	/*  Triangular window */
	else if( type[0] == 'T' ){

		center = (float)( wlen - 1 )/2. + (float)( fsamp );
		extent = center - (float)( fsamp );
		for( i = fsamp; i <= lsamp; i++ ){
			Y[i] = X[i]*(1. - fabs( (float)( i ) - center )/extent);
		}
	}

	/* Undefined window type */
	else{
		fstrncpy( err, err_s-1, " WINDOW -  Undefined window type ", 33 );
		return;
	}

	/*  Zero that part of the signal outside of the window
	 * */
	zero( &Y[1], fsamp - 1 );
	zero( &Y[lsamp + 1], n - lsamp );

	/*  Bye
	 * */
	return;
} /* end of function */

