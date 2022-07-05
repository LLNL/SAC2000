#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/mem.h"
#include "../../inc/contouring.h"
void /*FUNCTION*/ putcontseglabel(number, status, numlocs, firstloc)
int number, status, numlocs, firstloc;
{

        int *Isacmem;

	/*=====================================================================
	 * PURPOSE:  To put (store) label information about an existing
	 *           contouring segment information.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    number:    The segment number. [i]
	 *    status:    The labeling status for this segment. [i]
	 *    numlocs:   The number of labels for this segment. [i]
	 *    firstloc:  The pointer to the first label for this segment. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  contouring/5
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    contouring:  numsegments, indexseglabelst, 
	 *                 indexseglabelnu, indexseglabelfi
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    mem:         sacmem, isacmem
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900821:  Shortened variable names to 15 characters max, to keep
	 *             things working under SunOS 3.5:
	 *                indexseglabelstatus -> indexseglabelst
	 *                indexseglabelnumber -> indexseglabelnu
	 *                indexseglabelfirst  -> indexseglabelfi
	 *    900418:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900418
	 *===================================================================== */
	/* PROCEDURE: */
	if( number <= cmcontouring.numsegments ){
                Isacmem = (int *)cmmem.sacmem[cmcontouring.indexseglabelst];
		*(Isacmem + number - 1) = status;

                Isacmem = (int *)cmmem.sacmem[cmcontouring.indexseglabelnu];
		*(Isacmem + number - 1) = numlocs;

                Isacmem = (int *)cmmem.sacmem[cmcontouring.indexseglabelfi];
		*(Isacmem + number - 1) = firstloc;
		}
	else{
		fprintf( stdout, "Illegal labeled segment number: %d \n", 
		 number );
		}

L_8888:
	return;

} /* end of function */

