#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

#define MEMINIT 100

void /*FUNCTION*/ iniam(memstruct)
struct t_cmmem *memstruct; 
{

        int i;

	/*=====================================================================
	 * PURPOSE:  To initialize the array manager.
	 *=====================================================================
	 * INPUT ARGUMENTS:
         *    MEMSTRUCT:   Memory structure being managed. [f,i]
	 *  
	 *=====================================================================
	 * MODULE/LEVEL:  AM/3
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *
	 *=====================================================================
	 * METHODOLOGY:
	 *
	 *=====================================================================
	 * IMPLEMENTATION:
	 *=====================================================================
	 * USER CALLABLE SUBROUTINES:
	 *    INIAM:   Initializes the array manager.
	 *    ALLAMB:  Allocates a block from the array of requested size.
	 *    RELAMB:  Releases a previously allocated block.
	 *=====================================================================
	 * INTERNAL SUBROUTINES/FUNCTIONS:
	 *===================================================================== */
	/* PROCEDURE: */

        if ( memstruct->nallocated != 0 ){
/* not the first time in.  release previously allocated memory and reinitialize */

          for (i=0; i<memstruct->nallocated; i++){
                  free(memstruct->sacmem[i]);
                  memstruct->sacmem[i] = NULL;
		}
	}
        else {
/* first time in.  initialize the sacmem storage area.  allocate block of pointers */
          if((memstruct->sacmem =(float **)malloc(MEMINIT*sizeof(float *))) == NULL){
            printf("Error allocating initial memory-iniam\n  quitting\n");
            exit(1);
	  }
          memstruct->nallocated = MEMINIT;
          for (i=0; i<MEMINIT; i++){
            memstruct->sacmem[i] = NULL;
	  }
	}

	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    940127:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED: 
	 *===================================================================== */

} /* end of function */

