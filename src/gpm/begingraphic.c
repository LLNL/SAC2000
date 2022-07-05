#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ begingraphics(nerr)
int *nerr;
{

	/*=====================================================================
	 * PURPOSE:  To begin (initialize) the SAC Graphics Library.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  gpm/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  inigpm, inigtm, inigdm
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861027:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  861027
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Initialize the Graphics Plot Module. */

	/*      call inigpm */

	/* - Initialize the Graphics Tool Module. */

	inigtm();

	/* - Initialize the Graphics Device Module. */

	inigdm( nerr );

L_8888:
	return;

} /* end of function */

