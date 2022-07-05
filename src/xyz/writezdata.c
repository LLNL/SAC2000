#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
void /*FUNCTION*/ writezdata(filename, filename_s, zdata, nzsize, nerr)
char *filename;   int filename_s;
float zdata[];
int *nzsize, *nerr;
{
	int _l0, notused, nun;
	void zwabs();


	/* - Create file */

	znfile( &nun, filename,filename_s, "DATA",5, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Write data to file */

        _l0 = 0;
	zwabs( &nun, zdata, *nzsize, &_l0, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Close file. */

	zclose( &nun, &notused );

L_8888:
	return;

} /* end of function */


