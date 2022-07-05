#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/comlists.h"
void /*FUNCTION*/ setcomlist(number)
int number;
{



	/*=====================================================================
	 * PURPOSE:  To set the command list number, which defines
	 *           the set of commands that are available for execution.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    number:  Command list number to use. [i]
	 *             = 1  Standard SAC command list.
	 *             = 2  Spectral Estimation Subprocess (SES) command list.
	 *             = 3  Signal Stacking Subprocess (SSS) command list.
	 *====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    comlists:   icomlist
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900804:  Changed from clf to comlist.
	 *    861203:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861203
	 *===================================================================== */
	/* PROCEDURE: */
	if( number >= 1 && number <= 3 )
		cmcomlists.icomlist = number;

L_8888:
	return;

} /* end of function */

