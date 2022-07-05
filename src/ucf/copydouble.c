#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ copydouble(source, length, sink)
double source[];
int length;
double sink[];
{
	int j, j_;

	double *const Sink = &sink[0] - 1;
	double *const Source = &source[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To copy double-precision variables from one place to another.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    source:     Source array. Array to copy from. [d]
	 *    length:     Number of values to copy. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    sink:       Sink array.  Array to copy into. [d]
	 *=====================================================================
	 * MODULE/LEVEL:  ucf/4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890202:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890202
	 *===================================================================== */
	/* PROCEDURE: */
	for( j = 1; j <= length; j++ ){
		j_ = j - 1;
		Sink[j] = Source[j];
		}

L_8888:
	return;

} /* end of function */

