#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ reaamb(array, nsize, nosize, index, newndx, nerr)
float **array;
int nsize, nosize, index, *newndx, *nerr;
{

        float *tempptr;

	/*=====================================================================
	 * PURPOSE:  To reallocate a block of memory of requested size.
	 * 
	 * 
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ARRAY:   Pointer array being managed. [f,i]
	 *    NSIZE:   Size of requested block. [i]
	 *    NOSIZE:  Size of old block to copy. [i]
	 *    INDEX:   Index in ARRAY of beginning of old block. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NEWNDX:  Index in ARRAY of beginning of new block. [i]
	 *    NERR:    Error return flag.  Set to 0 if no error occurred.
	 *             If fails the new block is deallocated also.
	 *             = 0    no error occurred.
	 *             = 0301 no available block is large enough.
	 *=====================================================================
	 * MODULE/LEVEL:  AM/3
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - See INIAM for detailed description of array manager package.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920723:  Original version. (Amanda Goldner)
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  920723
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Allocate a new block */

        if((tempptr = (float *)realloc(array[index],nsize*sizeof(float))) == NULL){
          *nerr = 0301;
          *newndx = -1;
	}
        else {
          *newndx = index;
          array[index] = tempptr;
        }              

       
L_8888:
	return;

} /* end of function */

