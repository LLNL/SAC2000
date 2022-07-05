#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
double /*FUNCTION*/ dbh_random(seed)
int *seed;
{
	float random_v;



	*seed = 2045**seed + 1;
	*seed = *seed - (*seed/1048576)*1048576;
	random_v = (float)( *seed + 1 )/1048577.0;

	return( random_v );
} /* end of function */

