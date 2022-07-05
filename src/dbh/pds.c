#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*                                                                PDS
 *  Computes power density spectral estimate by indirect method, i.e.
 *    by taking the Fourier transform of a windowed sample correlation
 *    function.   See Jenkins and Watts for more details.
 *
 *
 *  Input Arguments:
 *  ----- ----------
 *
 *    R              REAL*4 array containing sample correlation function.
 *
 *    R_SIZE         Size of array R.
 *
 *    #LAGS          Number of correlation samples available.
 *
 *    WINDOW_SIZE    Correlation window size in samples.  An integer quantity.
 *
 *    WINDOW_TYPE    Character*8 variable containing ascii name for window.
 *
 *
 *    FFT_SIZE       Size of fft used for spectral computation.  An integer
 *                     quantity.
 *
 *  Output Arguments:
 *  ------ ----------
 *
 *    SPECTRUM       REAL*4 array  in which the spectral estimate is returned.
 *                   This array must be of length FFT_SIZE.  Upon return
 *                   from this routine the valid spectral samples occupy the
 *                   first FFT_SIZE/2 + 1 elements of this array, with the dc
 *                   value in SPECTRUM(1).
 *
 *    ERROR_MESSAGE  Character*130 string containing error message if an
 *                   error occurs, ' ' otherwise
 *
 *  Auxiliary Arguments:
 *  --------- ----------
 *
 *    AUXILIARY      Temporary real*4 work space large enough to hold the
 *                   intest allowable correlation function.  Also must be
 *                   at least as large as FFT_SIZE.
 *
 *  Linkage:  WINDOW, FFT, SHIFT, ZERO, ALIAS
 *
 *  Author:  David Harris
 *
 *  Created:  January 13, 1980
 *
 *  Last Modified:  January 4, 1985
 *
 * */
void /*FUNCTION*/ pds(r, nr, nlags, wlen, wtype, nfft, spectr, errmsg, 
	 errmsg_s, tempor)
float r[];
int nr, nlags, *wlen;
char *wtype;
int nfft;
float spectr[];
char *errmsg;   int errmsg_s;
float tempor[];
{
	char tempe[131];
	int fsamp, hsize, i, sympnt;
	float pi;

	float *const R = &r[0] - 1;
	float *const Spectr = &spectr[0] - 1;
	float *const Tempor = &tempor[0] - 1;



	/*  Enforce odd window length */
	*wlen = (*wlen/2)*2 + 1;

	/*  Check to be sure that the window size does not exceed the number
	 *    of available correlation function lags.	*/
	if( *wlen > nlags ){
	    fstrncpy( errmsg, errmsg_s-1, "PDS *** Correlation window length exceeds #lags ***"
	     , 51 );
	    return;
	}

	pi = 3.14159265;
	hsize = nfft/2 + 1;

	/*  If WINDOW_SIZE is larger than FFT_SIZE, it is necessary to form an
	 *    aliased version of the correlation sequence before transforming to
	 *    obtain the spectrum.  Thus, the correlation sequence is first windowed,
	 *    then aliased.
	 *
	 *    First rotate the correlation sequence by half its length
	 * */
	shift( r, nr, nlags/2, "Circular", tempor, errmsg,errmsg_s );
	if( !(memcmp(errmsg,"        ",8) == 0) ){

	    fstrncpy(tempe, 130, errmsg, strlen(errmsg));
	    fstrncpy(tempe+strlen(errmsg),130-strlen(errmsg)," (FROM PDS)", 11);
	    fstrncpy(errmsg,errmsg_s-1,tempe,strlen(tempe));
	    return;
	}
	sympnt = nlags/2 + 1;

	/*    Next window it */
	fsamp = sympnt - *wlen/2;
	window( tempor, nr, wtype, fsamp, *wlen, tempor, errmsg,errmsg_s );
	if( !(memcmp(errmsg,"        ",8) == 0) ){
	    fstrncpy(tempe, 130, errmsg, strlen(errmsg));
	    fstrncpy(tempe+strlen(errmsg),130-strlen(errmsg)," (FROM PDS)", 11);
	    fstrncpy(errmsg,errmsg_s-1,tempe,strlen(tempe));
	    return;
	}

	/*    Now alias the windowed correlation function */
	alias( tempor, sympnt, nr, nfft, spectr );

	/*  Compute Fourier transform */
	zero( tempor, nfft );
	fft( spectr, tempor, nfft, -1 );

	/*  Enforce positivity of result
	 * */
	for( i = 1; i <= hsize; i++ ){
	    if( Spectr[i] < 0. ){
		Spectr[i] = -Spectr[i];
	    }
	}

	/*  Bye */

	return;
} /* end of function */

