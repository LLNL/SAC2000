#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ fill(array, number, value)
float array[];
int number;
double value;
{
	int j, j_;

	float *const Array = &array[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To fill an array with a value.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    NUMBER:  Number of elements in array. [i]
	 *    VALUE:   Value to put in each element. [r]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ARRAY:   Output array. [r]
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *===================================================================== */
	/* PROCEDURE: */
	for( j = 1; j <= number; j++ ){
		j_ = j - 1;
		Array[j] = value;
		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    810000:  Original version.
	 *===================================================================== */

} /* end of function */

