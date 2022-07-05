#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lcrrcp(realmn, realmx, realv1, realv2)
double realmn, realmx;
float *realv1, *realv2;
{
	int lcrrcp_v;
	int nerr;
	float rv;


	/*=====================================================================
	 * PURPOSE: To parse a "range-checked real variable pair" construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lcrrcp:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    realmn:  Minimum allowed value for real variable. [f]
	 *    realmx:  Maximum allowed value for real variable. [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    realv1:   First real variable found. [f]
	 *    realv2:   Second real variable found. [f]
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
	 * DOCUMENTED/REVIEWED:  900510
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Get real variable from next symbol.
	 * - Check variable against allowed range.
	 * - Perform standard command error recovery if not found.
	 * - Repeat for second real. */
L_2000:
	if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
		lcrrcp_v = TRUE;
		rv = Flnum[cmcom.jcom];
		if( rv >= realmn && rv <= realmx ){
			*realv1 = rv;
			cmcom.jcom = cmcom.jcom + 1;
L_3000:
			if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
				rv = Flnum[cmcom.jcom];
				if( rv >= *realv1 && rv <= realmx ){
					*realv2 = rv;
					cmcom.jcom = cmcom.jcom + 1;
					}
				else{
					cfmt( "OUTSIDE ALLOWED RANGE:$",24 );
                                        fprintf(MUNOUT," Allowed range is: %16.5g%16.5g\n",
			                                                  *realv1, realmx );
					cresp();
					if( lcmore( &nerr ) )
						goto L_3000;
					lcrrcp_v = TRUE;
					}
				}
			else{
				cfmt( "NEED A REAL VARIABLE:$",23 );
				cresp();
				if( lcmore( &nerr ) )
					goto L_2000;
				lcrrcp_v = TRUE;
				}
			}
		else{
			cfmt( "OUTSIDE ALLOWED RANGE:$",24 );
                        fprintf(MUNOUT," Allowed range is: %16.5g%16.5g\n",
			                                   realmn, realmx );
			cresp();
			if( lcmore( &nerr ) )
				goto L_2000;
			lcrrcp_v = TRUE;
			}
		}
	else{
		lcrrcp_v = FALSE;
		}

L_8888:
	return( lcrrcp_v );

} /* end of function */

