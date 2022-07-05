#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*                                                           DIRCOR
 *
 *  Performs time-domain correlation directly from an input sequence.
 *
 *
 *  Author:  Dave Harris
 *
 *  Created:  December 6, 1984
 *
 *  Last Modified:  December 18, 1984
 *
 *  Input arguments:
 *  ----- ----------
 *
 *    DATA1                  REAL*4 Array holding first data sequence
 *
 *    DATA2                  REAL*4 Array holding second data sequence.
 *
 *    #SAMPLES               Integer variable containing number of samples
 *                           in data sequence.
 *
 *    DELAY                  Integer variable containing delay (shift) between
 *                           data sequences, if desired.  Zero if not desired.
 *
 *    #LAGS                  Integer variable containing number of correlation
 *                           lags to compute.
 *
 *
 *  Output Arguments:
 *  ------ ----------
 *
 *    C                      REAL*4 Array of correlation samples.
 *
 *  Linkage:  none
 *
 *
 * */
void /*FUNCTION*/ dircor(data1, data2, npts, delay, nlags, c)
float data1[], data2[];
int npts, delay, nlags;
double c[];
{
	int i, j;
	double temp;


	for( i = 0; i < nlags ; i++ ){
	    temp = 0.0e0;
	    for( j = i + delay ; j < npts; j++ )
		temp = temp + (double)data1[j]*(double)data2[j - i - delay];

	    c[i] = temp;
	}

	return;
} /* end of function */

