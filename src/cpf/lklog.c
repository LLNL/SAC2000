#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lklog(kkey, kkey_s, logv)
char *kkey;   int kkey_s;
int *logv;
{
	char ktoken[9];
	int lklog_v;



	/*=====================================================================
	 * PURPOSE: To parse a "keyed logical variable" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lklog:   .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [i]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    logv:    Logical variable found in command. [l]
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
	 *    sac:     lckey, modcase
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870730:  Added logic to convert current token to uppercase.
	 *    820423:  Adjustments due to new command parsing system.
	 *    820312:  Factored test for key to LCKEY function.
	 *    810207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for key. */
	lklog_v = lckey( kkey,kkey_s );

	/* - Copy current token to local storage and convert to uppercase. */

	modcase( TRUE, (char*)kmcom.kcom[cmcom.jcom - 1], MCPW, ktoken );

	/* - Check for "ON" or "OFF" at next token.
	 *   Set logical variable to .TRUE. if not found. */

	if( lklog_v ){
L_2000:
		if( memcmp(ktoken,"ON",2) == 0 ){
			*logv = TRUE;
			cmcom.jcom = cmcom.jcom + 1;
			}
		else if( memcmp(ktoken,"OF",2) == 0 ){
			*logv = FALSE;
			cmcom.jcom = cmcom.jcom + 1;
			}
		else{
			*logv = TRUE;
			}
		}

L_8888:
	return( lklog_v );

} /* end of function */

