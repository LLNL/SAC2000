#include <stdio.h>
#include <stdlib.h>
/* Given an array of floats, and the length of the array,
   set each element to zero. */

void /*FUNCTION*/ zero(a, n)
float a[];
int n;
{
	int k ;

	for( k = 0 ; k < n ; k++ )
	    a[ k ] = 0.0 ;

} /* end of function */

