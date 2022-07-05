#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ zcloses(nfu, nerr)
FILE **nfu;
int *nerr;
{
	/*=====================================================================
	 * PURPOSE:  To close a disk file if it is open.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    nfu:     Fortran file unit. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag.  Set to zero if no error. [i]
	 *=====================================================================
	 * SPECIAL NOTE:  It is NOT an error if no file is open on unit.
	 *=====================================================================
	 * MODULE/LEVEL:  co/5
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    880826:  Fixed bug with F77--a call to close when there is no
	 *             file open on file unit ties up that file unit. Added
	 *             a check to make sure file is open before calling close.
	 *    851216:  Modified for UNIX file I/O--D. Trimmer
	 *    830812:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  880115
	 *===================================================================== */
	/* PROCEDURE: */

        *nerr = fclose(*nfu);

} /* end of function */

