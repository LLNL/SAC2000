#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ ptp(signal, npts, length, ptpval, ipmin, ipmax)
float signal[];
int npts, *length;
float *ptpval;
int *ipmin, *ipmax;
{
	int i, i_, imax, imin, start, stop;
	float maxim, minim;

	float *const Signal = &signal[0] - 1;


	/*  PTP - Measures peak to peak excursion of a waveform by
	 *         finding the supremum of all peak to peak amplitudes
	 *         in all windows of the signal of a given length.
	 *
	 *  Author:  Dave Harris
	 *
	 *  Created:  February 10, 1982
	 *
	 *  Last modified: February 10, 1982
	 *
	 *  Input arguments:
	 *  ----- ----------
	 *
	 *    SIGNAL                 Array containing signal
	 *
	 *    NPTS                   # points in signal
	 *
	 *    LENGTH                 length of sliding window
	 *
	 *
	 *  Output Arguments:
	 *  ------ ----------
	 *
	 *    PTPVAL                 Measured value of peak to peak amplitude
	 *
	 *    IPMIN                  Index of p-p minimum
	 *
	 *    IPMAX                  Index of p-p maximum
	 *
	 *  Linkage:  (NONE)
	 *
	 *
	 * */

	/*  Check sliding window length, if inter than data length,
	 *    then reset to data length, if less than or equal to
	 *    zero, then set to half the data length.
	 * */
	if( *length > npts ){
		*length = npts;
		}
	else if( *length <= 0 ){
		*length = npts/2;
		}

	/*  Initializations
	 * */
	*ptpval = 0.;
	start = 1;
	stop = *length;

	/*  Main Loop
	 * */
L_1:
	;
	if( stop > npts )
		goto L_2;

	/*    Find min and max in window.
	 * */
	maxim = -1.e30;
	minim = 1.e30;

	for( i = start; i <= stop; i++ ){
		i_ = i - 1;

		if( Signal[i] > maxim ){
			maxim = Signal[i];
			imax = i;
			}
		if( Signal[i] < minim ){
			minim = Signal[i];
			imin = i;
			}

		/*               I */
		}

	/*    Check p-p in window against accumulating p-p
	 * */
	if( maxim - minim > *ptpval ){
		*ptpval = maxim - minim;
		*ipmax = imax;
		*ipmin = imin;
		}

	/*    Slide window
	 * */
	start = start + 1;
	stop = stop + 1;

	/*               Main loop */
	goto L_1;
L_2:
	;

	/*  Bye
	 * */
	return;
} /* end of function */

