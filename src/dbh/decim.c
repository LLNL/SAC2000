#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*  DECIM -- Efficient Time-Domain Decimation/Filtering Code
 *
 *  Author:  Dave Harris
 *
 *  Last Modified:  July 24, 1981
 *
 *
 *  Input Arguments:
 *  ----------------
 *
 *    X               Array containing input sequence.
 *
 *    NX              Length of X sequence.
 *
 *    BUF             Work space array.
 *
 *    NB              Length of BUF.  Must be >= NC.
 *
 *    C               Array of filter coefficients, stored
 *                      in symmetric mode.
 *
 *    NCHP1           Number of entries in C.
 *
 *    ISYM            Integer variable containing code for
 *                      symmetry type:
 *                        1 -- evenly symmetric
 *                       -1 -- odd symmetry
 *
 *    IRATE           Integer desampling rate.
 *
 *  Output Arguments:
 *  -----------------
 *
 *    Y               Array containing decimated/filtered sequence.
 *                      May be the same array as X for in-place
 *                      computations.
 *
 *    NY              Length of Y sequence.
 *
 *
 *  Linkage:  (None)
 * */
void /*FUNCTION*/ decim(x, nx, buf, nb, c, nchp1, isym, irate, y, ny)
float x[];
int nx;
float buf[];
int nb;
float c[];
int nchp1, isym, irate;
float y[];
int *ny;
{
	int i, in, ioff, nbstop, nc, nch, nex, nsave, nxstop, 
	 out, ptr;
	float temp;

	float *const Buf = &buf[0] - 1;
	float *const C = &c[0] - 1;
	float *const X = &x[0] - 1;
	float *const Y = &y[0] - 1;




	/*  Initializations */
	nch = nchp1 - 1;
	nc = 2*nch + 1;
	*ny = (nx - 1)/irate + 1;
	nex = nb - nc;
	nex = (nex/irate)*irate;
	nxstop = nx - nch - irate;
	nbstop = nchp1 + nex;
	ptr = nchp1 - irate;
	in = 1 - irate;
	out = 0;

	/*  Initialize buffer, with zeroes where no data available. */
	for( i = 1; i <= nch; i++ )
	    Buf[i] = 0.;

	for( i = nchp1; i <= (nc - irate); i++ )
	    Buf[i] = X[i - nch];

	/*  Main loop - filter until end of data reached. */
	while ( in <= nxstop ) {

	    /*    Shift buffer, if end of buffer reached. */
	    if( ptr == nbstop ){
		ptr = nchp1 - irate;
		nsave = nc - irate;
		ioff = nex + irate;
		for( i = 1; i <= nsave; i++ )
		    Buf[i] = Buf[i + ioff];
	    }

	    /*    Update pointers */
	    out = out + 1;
	    in = in + irate;
	    ptr = ptr + irate;

	    /*    Read in new data */
	    for( i = 1; i <= irate; i++ ){
		ioff = nch + i - irate;
		Buf[ptr + ioff] = X[in + ioff];
	    }

	    /*    Compute output point */
	    temp = C[1]*Buf[ptr];
	    for( i = 1; i <= nch; i++ )
		temp = temp + C[i + 1]*(Buf[ptr + i] + isym*Buf[ptr - i]);

	    Y[out] = temp;
	} /* end while */

	/*  Loop to handle case where operator runs off of the data */
	while ( in <= nx - irate ) {

	    /*    Shift buffer, if end of buffer reached. */
	    if( ptr == nbstop ){
		ptr = nchp1 - irate;
		nsave = nc - irate;
		ioff = nex + irate;
		for( i = 1; i <= nsave; i++ )
		    Buf[i] = Buf[i + ioff];
	    }

	    /*    Update pointers */
	    out = out + 1;
	    in = in + irate;
	    ptr = ptr + irate;

	    /*    Read in new data */
	    for( i = 1; i <= irate; i++ ){
		ioff = nch + i - irate;
		if( in + ioff > nx )
		    Buf[ptr + ioff] = 0.;

		else
		    Buf[ptr + ioff] = X[in + ioff];

	    }

	    /*    Compute output point */
	    temp = C[1]*Buf[ptr];
	    for( i = 1; i <= nch; i++ )
		temp = temp + C[i + 1]*(Buf[ptr + i] + isym*Buf[ptr - i]);

	    Y[out] = temp;
	} /* end while */


	/*  Done */
	return;
} /* end of function */

