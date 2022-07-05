#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/eam.h"
void /*FUNCTION*/ chpf(nerr)
int *nerr;
{


	/* Ind
	 *=====================================================================
	 * PURPOSE:  To close the HYPO pick file (HPF) if open.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  EAM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    EAM:     LHPFOP, NHPFUN
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    EAM:     LHPFOP
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ZCLOSE
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920505:  Added hypo input file end-of-file identifier string.
	 *    820810:  Documented subroutine.
	 *    820303:  Only call ZCLOSE if LHPFOP is .TRUE.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Close HYPO pick file if open. Write hypo eof string first. */

	if( cmeam.lhpfop ){
                fprintf(cmeam.nhpfun,"%19s\n","10");
		zcloses( &cmeam.nhpfun, nerr );
		}

	/* - Set flag to show that HPF is closed. */

	cmeam.lhpfop = FALSE;

L_8888:
	return;


} /* end of function */

