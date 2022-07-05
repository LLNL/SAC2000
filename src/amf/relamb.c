#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ relamb(array, index, nerr)
float **array;
int index, *nerr;
{


	/*=====================================================================
	 * PURPOSE:  To release a previously allocated block of memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ARRAY:   Pointer array being managed.
	 *    INDEX:   Index into pointer array.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag.  Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  AM/3
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - See INIAM for detailed description of array manager package.
	 *====================================================================*/
	/* PROCEDURE: */
/*        printf("relamb releasing index %ld \n",index); */

	*nerr = 0;

        free(array[index]);

        array[index] = NULL;

	return;

} /* end of function */

