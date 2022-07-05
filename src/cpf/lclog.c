#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lclog(logv)
int *logv;
{
	char ktoken[9];
	int lclog_v;



	/*=====================================================================
	 * PURPOSE: To parse a "logical variable" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lclog:   .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [i]
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
	 * MODIFICATION HISTORY:
	 *    870730:  Added logic to convert current token to uppercase.
	 *    820423:  Adjustments due to new command parsing system.
	 *    820312:  Factored test for key to LCCKEY function.
	 *    810207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Copy current token to local storage and convert to uppercase. */
	modcase( TRUE, (char*)kmcom.kcom[cmcom.jcom - 1], MCPW, ktoken );

	/* - Check for "ON" or "OFF" at next token.
	 * - Do not change value of logical variable if not found. */

	if( memcmp(ktoken,"ON",2) == 0 ){
		*logv = TRUE;
		cmcom.jcom = cmcom.jcom + 1;
		lclog_v = TRUE;
		}
	else if( memcmp(ktoken,"OF",2) == 0 ){
		*logv = FALSE;
		cmcom.jcom = cmcom.jcom + 1;
		lclog_v = TRUE;
		}
	else{
		lclog_v = FALSE;
		}

L_8888:
	return( lclog_v );

} /* end of function */

