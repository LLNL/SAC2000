#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lkrrcp(kkey, kkey_s, realmn, realmx, realv1, 
	 realv2)
char *kkey;   int kkey_s;
double realmn, realmx;
float *realv1, *realv2;
{
	int lkrrcp_v;
	int nerr;
	float realm, rv;


	/*=====================================================================
	 * PURPOSE: To parse a "keyed range-checked real variable pair" construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lkrrcp:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *    realmn:  Minimum allowed value for real variable. [f]
	 *    realmx:  Maximum allowed value for real variable. [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    realv1:   First real found. [f]
	 *    realv2:   Second real found. [f]
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
	 * SUBROUTINES 
	 *    sac:  lckey, cfmt, cresp, lcmore
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820423:  Adjustments due to new command parsing system.
	 *    820312:  Factored test for key to LCCKEY function.
	 *    810207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for key. */
	lkrrcp_v = lckey( kkey,kkey_s );

	/* - If key was found:
	 * -- Get real variable from next symbol.
	 * -- Check variable against allowed range.
	 * -- Perform standard command error recovery if not found.
	 * -- Repeat for second real. */

	if( lkrrcp_v ){
L_2000:
		if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
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
						} /* end if( rv >= *realv1 && rv <= realmx ) */
					else{
						cfmt( "OUTSIDE ALLOWED RANGE:$",24 );
                                                fprintf(MUNOUT," Allowed range is: %16.5g%16.5g\n",
						                                  *realv1, realmx );
						cresp();
						if( lcmore( &nerr ) )
							goto L_3000;
						} /* end else associated with if( rv >= *realv1 && rv <= realmx ) */
					} /* end inner if( Itypcm[cmcom.jcom] == cmcom.inumbr ) */
				else{
					cfmt( "NEED A REAL VARIABLE:$",23 );
					cresp();
					if( lcmore( &nerr ) )
						goto L_2000;
					} /* end else associated with inner if( Itypcm[cmcom.jcom] == cmcom.inumbr ) */
				} /* end if( rv >= realmn && rv <= realmx ) */
			else{
				cfmt( "OUTSIDE ALLOWED RANGE:$",24 );
                                fprintf(MUNOUT," Allowed range is: %16.5g%16.5g\n",
				                                    realmn, realmx );
				cresp();
				if( lcmore( &nerr ) )
					goto L_2000;
				} /* end else associated with if( rv >= realmn && rv <= realmx ) */
			} /* end outer if( Itypcm[cmcom.jcom] == cmcom.inumbr ) */
		else{
			cfmt( "NEED A REAL VARIABLE:$",23 );
			cresp();
			if( lcmore( &nerr ) )
				goto L_2000;
			} /* end else associated with outer if( Itypcm[cmcom.jcom] == cmcom.inumbr ) */
		} /* end if( lkrrcp_v ) */

L_8888:
	return( lkrrcp_v );

} /* end of function */

