#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lcirc(intmn, intmx, intv)
int intmn, intmx, *intv;
{
	int lcirc_v;
	int iv, nerr;


	/*=====================================================================
	 * PURPOSE: To parse a "range-checked integer variable" construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    LCIRC:   .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [i]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    intmn:   Minimum allowed value for integer variable. [i]
	 *    intmx:   Maximum allowed value for integer variable. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    intv:    Integer variable found in command. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MUNOUT
	 *    com:     jcom, ncom, kcom, itypcm, inumbr, flnum
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     jcom
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     cfmt, cresp, lcmore
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820423:  Adjustments due to new command parsing system.
	 *    810207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* -- Get integer variable from next symbol.
	 * -- Check variable against allowed range.
	 * -- Perform standard command error recovery if out of range. */
L_2000:
	if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
		iv = (int)( Flnum[cmcom.jcom] + 0.1 );
		if( iv >= intmn && iv <= intmx ){
			*intv = iv;
			cmcom.jcom = cmcom.jcom + 1;
			lcirc_v = TRUE;
			}
		else{
			cfmt( "OUTSIDE ALLOWED RANGE:$",24 );
                        fprintf(MUNOUT," Allowed range is: %10d%10d\n",intmn, intmx );
			cresp();
			if( lcmore( &nerr ) )
				goto L_2000;
			lcirc_v = TRUE;
			}
		}
	else{
		lcirc_v = FALSE;
		}

L_8888:
	return( lcirc_v );

} /* end of function */

