#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

#define MEMINC 50

void /*FUNCTION*/ allamb(memstruct, nsize, index, nerr)
struct t_cmmem *memstruct;
int nsize, *index, *nerr;
{

        float **tempptr;
        int i, j, ierr;

	/*=====================================================================
	 * PURPOSE:  To allocate a block of memory of requested size.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    MEMSTRUCT:   Memory structure being managed. [f,i]
	 *    NSIZE:   Size of requested block. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    INDEX:   Index in MEMSTRUCT->SACMEM (pointer to requested memory).
	 *    NERR:    Error return flag.  Set to 0 if no error occurred.
	 *             = 0    no error occurred.
	 *             = 0301 error allocating memory
	 *=====================================================================
	 * MODULE/LEVEL:  AM/3
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - See INIAM for detailed description of array manager package.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    960709:  Changed malloc to calloc to initialize to zero, maf
	 *    940127:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
        ierr = 0;

	/* find an available pointer to use */
	/* SAC HACK: start at sacmem[1] rather than sacmem[0] to avoid */
	/* handing out index of zero, because this has historical      */
	/* significance throughout the code */

        for(i=1; (i<memstruct->nallocated && memstruct->sacmem[i] != NULL); i++)
	{ /* do nothing */ }

        if(i == memstruct->nallocated){
	/* there were no available entries, expand pointer array in place */

#ifdef DEBUG
        printf("expanding sacmem array\n");
#endif

          if((tempptr = (float **)realloc(memstruct->sacmem,
                      (memstruct->nallocated+MEMINC)*sizeof(float *))) == NULL)
            ierr = 1;
          else {
            memstruct->sacmem = tempptr;
            memstruct->nallocated += MEMINC;
            for (j=i; j<memstruct->nallocated; j++)memstruct->sacmem[j] = NULL;
	  }
	}

	/* Changed malloc to calloc to initialize to zero.  maf 960709 */
        if((memstruct->sacmem[i] = (float*)calloc(nsize,sizeof(float))) == NULL)
          ierr = 1;
        else
          *index = i;


        if( ierr != 0 ) {
#ifdef DEBUG
          printf("something has gone wrong in allamb--in error logic\n");
          printf("malloc failed for request of %d words\n",nsize);
#endif          
	  *index = 0;
	  *nerr = 301;
	  setmsg( "ERROR", *nerr );
	  apimsg( nsize );
        }

#ifdef DEBUG
/*        printf("allamb returning index %ld \n",*index); */
#endif

} /* end of function */

