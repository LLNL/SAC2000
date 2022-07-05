#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ xrcor(nerr)
int *nerr;
{

	*nerr = 1115;
	setmsg( "ERROR", *nerr );
	apcmsg( "READCOR",8 );

L_8888:
	return;

} /* end of function */

