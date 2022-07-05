#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ plrest()
{
	int j, j_;

	/* ind
	 *=====================================================================
	 * PURPOSE:  To restore the previously saved graphics environment.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     MCMGEM, MKMGEM, CMGEMS(), KMGEMS()
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     All.
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - PLSAVE must have been called prior to call to PLREST.
	 *===================================================================== */
	/* PROCEDURE: */


	if( lgems ){
          memcpy((char *)&cmgem.lxlim,(char *)&cmgemsav.lxlim,sizeof (struct t_cmgem));
          memcpy(&kmgem.ksides,&kmgemsav.ksides,sizeof (struct t_kmgem));
          lgems = FALSE;
        }
        
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    840118:  Modified method used to save and restore environment.
	 *    820831:  Changes due to modification of GEM common blocks.
	 *===================================================================== */

} /* end of function */

