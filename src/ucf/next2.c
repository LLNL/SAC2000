#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
int /*FUNCTION*/ next2(int num)
{
	int result = 2;
	while(result < num ){
		result *= 2;
	}
	return result;
} /* end of function */

