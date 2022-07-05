#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lcia(mnint, mxint, ia, nia)
int mnint, mxint, ia[], *nia;
{
	int lcia_v;
	int intv, nerr;

	int *const Ia = &ia[0] - 1;


	/*=====================================================================
	 * PURPOSE: To parse a "integer array" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lcia:    .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [i]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    mnint:   The minimum number of integers to return. [i]
	 *    mxint:   The maximum number of integers to return. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ia:    Array of integers found. [ia]
	 *    nia:   The number of integers returned. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom, ncom, kcom, itypcm, inumbr, ialpha, flnum
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     jcom
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lckey, cfmt, cresp
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820624:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Get integer variables until maximum is reached or
	 *   until an alphanumeric token is found.
	 * - Perform standard command error recovery if not found. */
	*nia = 0;
	lcia_v = TRUE;
L_2000:
	if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
		if( *nia < mxint ){
			intv = (int)( Flnum[cmcom.jcom] + 0.1 );
			*nia = *nia + 1;
			Ia[*nia] = intv;
			cmcom.jcom = cmcom.jcom + 1;
			}
		else{
			goto L_8888;
			}
		}
	else if( *nia >= mnint ){
		goto L_8888;
		}
	else if( *nia > 0 ){
		cfmt( "NEED A INTEGER VARIABLE:$",26 );
		cresp();
		if( lcmore( &nerr ) )
			goto L_2000;
		lcia_v = TRUE;
		goto L_8888;
		}
	else{
		lcia_v = FALSE;
		goto L_8888;
		}
	if( cmcom.jcom <= cmcom.ncom )
		goto L_2000;

L_8888:
	return( lcia_v );

} /* end of function */

