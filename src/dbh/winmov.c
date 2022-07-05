#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*                                                           WINMOV
 *
 *  Author:  Dave Harris
 *           L-205
 *           Lawrence Livermore National Laboratory
 *           P. O. Box 808
 *           Livermore, CA  94550
 *
 *  Created:  December 27, 1984
 *
 *  Last Modified:  December 27, 1984
 *
 *  Input arguments:
 *  ----- ----------
 *
 *    DATA                 Array containing input data sequence
 *
 *    WINDOW_LENGTH        Length of window.
 *
 *    W                    Array containing window sequence.
 *
 *  Output Arguments:
 *  ------ ----------
 *
 *    WINDOWED_DATA        Array containing windowed sequence
 *
 *
 *  Linkage:  None
 *
 *  Replacements:
 *
 * */
void /*FUNCTION*/ winmov(data, wlen, w, wdata)
float data[];
int wlen;
float w[], wdata[];
{
	int i;

	for( i = 0; i < wlen; i++ ){
		wdata[i] = w[i]*data[i];
	}

} /* end of function */

