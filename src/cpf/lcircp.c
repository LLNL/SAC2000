#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lcircp(intmn, intmx, intv1, intv2)
int intmn, intmx, *intv1, *intv2;
{
	int lcircp_v;
	int iv, nerr;


	/*=====================================================================
	 * PURPOSE: To parse a "range-checked real variable pair" construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    LCIRCP:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    intmn:   Minimum allowed value for real variable. [i]
	 *    intmx:   Maximum allowed value for real variable. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    intv1:   First integer variable found. [i]
	 *    intv2:   Second integer variable found. [i]
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
	 *    820622:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820622
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Get real variable from next symbol.
	 * - Check variable against allowed range.
	 * - Perform standard command error recovery if not found.
	 * - Repeat for second real. */
L_2000:
	if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
		lcircp_v = TRUE;
		iv = (int)( Flnum[cmcom.jcom] + 0.1 );
		if( iv >= intmn && iv <= intmx ){
			*intv1 = iv;
			cmcom.jcom = cmcom.jcom + 1;
L_3000:
			if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
				iv = (int)( Flnum[cmcom.jcom] + 0.1 );
				if( iv >= *intv1 && iv <= intmx ){
					*intv2 = iv;
					cmcom.jcom = cmcom.jcom + 1;
					}
				else{
					cfmt( "OUTSIDE ALLOWED RANGE:$",24 );
                                        fprintf(MUNOUT," Allowed range is: %10d%10d\n",
				                                          *intv1, intmx );
					cresp();
					if( lcmore( &nerr ) )
						goto L_3000;
					lcircp_v = TRUE;
					}
				}
			else{
				cfmt( "NEED A INTEGER VARIABLE:$",26 );
				cresp();
				if( lcmore( &nerr ) )
					goto L_2000;
				lcircp_v = TRUE;
				}
			}
		else{
			cfmt( "OUTSIDE ALLOWED RANGE:$",24 );
                        fprintf(MUNOUT," Allowed range is: %10d%10d\n",
     			                                    intmn, intmx );
			cresp();
			if( lcmore( &nerr ) )
				goto L_2000;
			}
		}
	else{
		lcircp_v = FALSE;
		}

L_8888:
	return( lcircp_v );

} /* end of function */

