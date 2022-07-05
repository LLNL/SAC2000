#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lklogr(kkey, kkey_s, logv, realv)
char *kkey;   int kkey_s;
int *logv;
float *realv;
{
	char ktoken[9];
	int lklogr_v;
	int nerr;



	/*=====================================================================
	 * PURPOSE: To parse a "keyed logical/real variable" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lklogr:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    logv:    Logical variable found in command. [l]
	 *    realv:   Integer variable found in command. [f]
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
	 *    sac:     lckey, modcase, cfmt, cresp
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870929:  Changes to allow both upper and lower case tokens.
	 *    820721:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870929
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for key. */
	lklogr_v = lckey( kkey,kkey_s );

	/* - Get logical variable from next symbol if key was found.
	 * - Perform standard error recovery if not found. */

	if( lklogr_v ){
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
			*realv = Flnum[cmcom.jcom];
			*logv = TRUE;
			cmcom.jcom = cmcom.jcom + 1;
			}
		else{
			cfmt( "NEED AN \"ON\", AN \"OFF\", OR A REAL:$",36 );
			cresp();
			if( lcmore( &nerr ) )
				goto L_2000;
			}
		}

	return( lklogr_v );
} /* end of function */

