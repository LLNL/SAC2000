#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/mem.h"
#include "../../inc/contouring.h"
void /*FUNCTION*/ getcontrlink(number, rlink)
int number, *rlink;
{
        int *Isacmem;


	/*=====================================================================
	 * PURPOSE:  To get the reverse link for an existing 
	 *           contouring line point.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    number:  The point number. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    rlink:   The reverse link to the previous point in segment. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  contouring/5
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    contouring:   numpoints, indexrlinks
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900412:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900412
	 *===================================================================== */
	/* PROCEDURE: */
	if( number <= cmcontouring.numpoints ){
                Isacmem = (int*)cmmem.sacmem[cmcontouring.indexrlinks];
		*rlink = *(Isacmem + number - 1);
	}
	else{
		fprintf( stdout, "Illegal point number:%d getcontrlink\n", number );
		exit(0);
	}

L_8888:
	return;

} /* end of function */

