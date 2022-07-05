#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lklogi(kkey, kkey_s, logv, intv)
char *kkey;   int kkey_s;
int *logv;
int *intv;
{
	char ktoken[9];
	int lklogi_v;
	int nerr;



	/*=====================================================================
	 * PURPOSE: To parse a "keyed logical/integer variable" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lklogi:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [i]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for.  [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    logv:    Logical variable found in command. [l]
	 *    intv:    Integer variable found in command. [i]
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
	 *    sac:      lckey, modcase, cfmt, cresp
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870929:  Changes to allow both upper and lower case tokens.
	 *    820721:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870929
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for key. */
	lklogi_v = lckey( kkey,kkey_s );

	/* - Get logical variable from next symbol if key was found.
	 * - Perform standard error recovery if not found. */

	if( lklogi_v ){
		modcase( TRUE, (char*)kmcom.kcom[cmcom.jcom - 1], MCPW, ktoken );
L_2000:
		if( memcmp(ktoken,"ON",2) == 0 ){
			*logv = TRUE;
			cmcom.jcom = cmcom.jcom + 1;
			}
		else if( memcmp(ktoken,"OF",2) == 0 ){
			*logv = FALSE;
			cmcom.jcom = cmcom.jcom + 1;
			}
		else if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
			*intv = (int)( Flnum[cmcom.jcom] + 0.1 );
			*logv = TRUE;
			cmcom.jcom = cmcom.jcom + 1;
			}
		else{
			cfmt( "NEED AN \"ON\", AN \"OFF\", OR AN INTEGER:$",40 );
			cresp();
			if( lcmore( &nerr ) )
				goto L_2000;
			}
		}


	return( lklogi_v );
} /* end of function */

