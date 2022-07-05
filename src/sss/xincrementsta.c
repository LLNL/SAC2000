#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/sss.h"
void /*FUNCTION*/ xincrementsta(nerr)
int *nerr;
{
	int jdfl, jdfl_, jvm, jvm_;



	/*=====================================================================
	 * PURPOSE:  To execute the INCREMENTSTACK command.  This command 
	 *           increments certain signal stack parameters.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  sss/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    dfm:     ndfl
	 *    sss:     dlyti, dlyni, vappi, t0vmi
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    sss:     dlyt, dlyn, vapp, t0vm
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    821201:  Minor cleanup and documentation.
	 *    810120:  Changed to output message retrieval from disk.
	 *    790831:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850819
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Increment static delays. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		jdfl_ = jdfl - 1;
		Dlyt[jdfl] = Dlyt[jdfl] + Dlyti[jdfl];
		Dlyn[jdfl] = Dlyn[jdfl] + Dlyni[jdfl];
		}

	/* - Increment velocity model parameters. */

	for( jvm = 1; jvm <= MVM; jvm++ ){
		jvm_ = jvm - 1;
		Vapp[jvm] = Vapp[jvm] + Vappi[jvm];
		T0vm[jvm] = T0vm[jvm] + T0vmi[jvm];
		}

L_8888:
	return;

} /* end of function */

