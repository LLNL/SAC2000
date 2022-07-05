#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/cpf.h"
void /*FUNCTION*/ setmacrolevel(imacrolevel)
int imacrolevel;
{

	/*=====================================================================
	 * PURPOSE:  To set the macro (nesting) level.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    imacrolevel:  Macro nesting level. [i]
	 *                  = 0 for terminal input. 
	 *                  > 0 for levels of macro nesting.
	 *=====================================================================
	 * MODULE/LEVEL:    cpf/4
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    cpf:   nmacrolevel, kvarsname
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900207
	 *===================================================================== */
	/* - Store input value in common block. */
	cmcpf.nmacrolevel = max( imacrolevel, 0 );

	/* - Create next name for vars list to store macro information. */

        sprintf(kmcpf.kvarsname,"macro%3.3d",cmcpf.nmacrolevel );

L_8888:
	return;

} /* end of function */

