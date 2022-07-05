#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*/cdoc 
 * /cdoc
 * /cdoc Name:     CALCFFTSIZE
 * /cdoc 
 * /cdoc
 * /cdoc Summmary:  Calculate various storage sizes for
 * /cdoc            FFT calculations.
 * /cdoc
 * /cdoc 
 * /cdoc
 * /cdoc Usage/Calling sequence:
 * /cdoc 
 * /cdoc    ierr = CALCFFTSIZE ( delta,sliceint,window,iorfft,lfft
 * /cdoc                         nptswndw,buffersize,windowovrl )
 * /cdoc 
 * /cdoc
 * /cdoc Arguments:
 * /cdoc
 * /cdoc    delta       =:    sampling interval - sac/samples(real)
 * /cdoc    sliceint    =:    image slice interval - seconds (real)
 * /cdoc    window      =:    data window size in seconds(real)
 * /cdoc    iorfft       :=   log base 2 of lfft(int)
 * /cdoc    lfft         :=   size of data for fft calc - must be power of 2(int)
 * /cdoc    nptswndw     :=   # pts in window
 * /cdoc    buffersize   :=   size of buffer to read in data(int)
 * /cdoc    windowovrl   :=   overlap of window(int)
 * /cdoc
 * /cdoc 
 * /cdoc  Returns:
 * /cdoc 
 * /cdoc      calcfftsize = 0 no error
 * /cdoc      calcfftsize > 0 error
 * /cdoc
 * /cdoc  Notes:
 * /cdoc
 * /cdoc   
 * /cdoc       By:    T.M.Quinn
 * /cdoc       On:    12/18/89
 * /cdoc 
 * /cdoc       Updates:
 * /cdoc 
 * /cdoc            By:
 * /cdoc            On:
 * /cdoc            Subj:
 * /cdoc 
 * /cdoc            By:
 * /cdoc            On:
 * /cdoc            Subj:
 * /cdoc 
 * /cdoc  */

#include "../../inc/spectrogram.h"
int /*FUNCTION*/ calcfftsize(delta, sliceint, window, iorfft, lfft, 
	 nptswndw, buffersize, windowovrl)
double delta, sliceint, window;
int *iorfft, *lfft, *nptswndw, *buffersize, *windowovrl;
{
	int calcfftsize_v, n;

	/*     * Include Files: */
	/*     * Local Variables: */
	/*     * Code Implementation: */
	calcfftsize_v = 1;

	/*         Convert window from seconds to number of points */
	*nptswndw = window/delta;

	/*         Calculate FFT size */
	*lfft = 2;
	*iorfft = 1;

	/*          FFT size must be at least twice the size of the window */
L_1:
	;
	if( *lfft < 2**nptswndw ){
		*lfft = *lfft*2;
		*iorfft = *iorfft + 1;
		goto L_1;
		}

	/*         Calculate window overlap from 'sliceint' */
	*windowovrl = *nptswndw - sliceint/(delta + .0000001);

	/*         Calculate optimum buffersize for reading in data */
	n = 1;
L_3:
	;
	if( (MAXBUFSIZE - *windowovrl)/(*nptswndw - *windowovrl) > n ){
		n = n + 1;
		goto L_3;
		}
	*buffersize = n**nptswndw - (n - 1)**windowovrl;

	/*         Check if FFT size is 'legal' */
	if( *lfft > MAXLFFT ){
		fprintf( stdout, "Error fft size greater than the largest allowed of %d .\n", 
		 MAXLFFT );
		}
	else{
		fprintf( stdout, "Window size: %d  Overlap: %d  FFT size: %d \n", 
		 *nptswndw, *windowovrl, *lfft );
		calcfftsize_v = 0;
		}


	return( calcfftsize_v );
} /* end of function */

