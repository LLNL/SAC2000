#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*                                                              PEF
 *
 *  Prediction error filter, with options for adaptive correction
 *    of coefficients, in-place processing.  Buffered for in-place
 *    processing.
 *
 *  Author:  Dave Harris
 *
 *  Created:  December 1, 1981
 *
 *  Last Modified:  April 26, 1984
 *
 *  Replacements
 *
 *
 *  Input Arguments:
 *  ----- ----------
 *
 *    DATA                   Real*4 input data array.
 *
 *    #POINTS                Number of data points.
 *
 *    A                      Real*4 array of filter coefficients.
 *
 *    FILTER_SIZE            Number of filter coefficients.
 *
 *    DELAY                  Operator delay (number of samples).
 *
 *    MU                     Real*4 Widrow algorithm adaptation constant
 *
 *
 *  Output Arguments:
 *  ------ ----------
 *
 *    RESULT                 Filtered output placed in this array.
 *                           May be the same array as DATA for in-place
 *                           filtering.
 *
 *    ERROR_MESSAGE          Character*130 error message variable.
 *
 *
 *  Linkage:  COPY, ZERO
 *
 * */
struct t_pefcom {
	float inbuf[2000], outbuf[2000];
} pefcom;
void /*FUNCTION*/ pef(data, npts, a, nc, delay, mu, result, errmsg)
float data[];
int npts;
float a[];
int nc, delay;
double mu;
float result[];
char *errmsg;
{
	int bufptr, datptr, k, lsamp, memory, ncmp, nextra, vrtptr;
	double e;

	float *const A = &a[0] - 1;
	float *const Data = &data[0] - 1;
	float *const Inbuf = &pefcom.inbuf[0] - 1;
	float *const Outbuf = &pefcom.outbuf[0] - 1;
	float *const Result = &result[0] - 1;



	/*  Declarations
	 * */


	/*  Initializations
	 * */
	datptr = 1;
	vrtptr = 1;
	memory = nc + delay;
	nextra = 2000 - memory;

	if( nextra < 1 )
		strcpy( errmsg, " PEF - Delay and/or filter too large " ) ;
	else
                errmsg[ 0 ] = '\0' ;

	zero( pefcom.inbuf, 2000 );
	zero( pefcom.outbuf, 2000 );

	/*  Loop
	 * */
L_1:
	;
	if( datptr >= npts )
		goto L_2;

	/*    Shift input buffer points back
	 * */
	copy( (int*)&Inbuf[nextra + 1], (int*)&Inbuf[1], memory );

	/*    Calculate start and stop points for buffer index.
	 * */
	bufptr = memory + 1;
	lsamp = min( 2000, npts - datptr + 1 + memory );
	ncmp = lsamp - bufptr + 1;

	/*    Load new data into input buffer
	 * */
	copy( (int*)&Data[datptr], (int*)&Inbuf[memory + 1], ncmp );

	/*    Filter data.
	 * */
L_3:
	;
	if( bufptr > lsamp )
		goto L_4;

	e = Inbuf[bufptr];

	for( k = 1; k <= nc; k++ ){
		e = e - A[k]*Inbuf[bufptr - delay - k];
	}

	/*    Widrow algorithm to correct filter coefficients
	 * */
	if( mu != 0. ){

		/*  Correction made only when filter memory extends onto data
		 * */
		if( vrtptr > memory ){
			for( k = 1; k <= nc; k++ ){
				A[k] = A[k] + 2.*mu*e*Inbuf[bufptr - delay - k];
			}
		}

	}

	Outbuf[bufptr] = e;

	bufptr = bufptr + 1;
	vrtptr = vrtptr + 1;

	goto L_3;
L_4:
	;

	/*    Store newly filtered data.
	 * */
	copy( (int*)&Outbuf[memory + 1], (int*)&Result[datptr], ncmp );

	/*    Increment data pointer.
	 * */
	datptr = datptr + ncmp;

	goto L_1;
L_2:
	;

	/*  Done
	 * */
	return;
} /* end of function */

