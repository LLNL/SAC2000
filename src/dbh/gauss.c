#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ gauss(seed, v1, v2)
int *seed;
float *v1, *v2;
{
	double x, y, z;



	x = dbh_random( seed );
	y = dbh_random( seed );
	z = sqrt( -2.0e0*log( x ) );

	*v1 = z*cos( 6.2831853*y );
	*v2 = z*sin( 6.2831853*y );

	return;
} /* end of function */

