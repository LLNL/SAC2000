#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ initializevars()
{



	/*=====================================================================
	 * PURPOSE:  To initialize the VARS Access Library.
	 *=====================================================================
	 * MODULE/LEVEL:  vars/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    lvarsinit
	 *    mem:     MMEM, MEPSL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    lvarsinit
	 *    mem:     sacmem
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  inivars, inimsg, iniam
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920409:  Moved lvarsinit to initcommon block data for initialization.
	 *    890227:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890227
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Return immediately if vars has already been initialized. */
	if( cmvars.lvarsinit )
		goto L_8888;

	/* - Initialize vars common block. */

	inivars();

	/* - Initialize message handling subsystem. */

	inimsg();

	/* - Initialize sacmem array for vars list storage. */

	iniam(&cmmem);

	/* - Set initialization flag. */

	cmvars.lvarsinit = TRUE;

L_8888:
	return;

} /* end of function */

