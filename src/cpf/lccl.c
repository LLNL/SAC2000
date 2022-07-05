#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lccl(kcl, kcl_s, ncl)
char *kcl;   int kcl_s;
int *ncl;
{
	char ktoken[MCPFN+1];
	int lccl_v;
	int ncname, nerr;



	/*=====================================================================
	 * PURPOSE: To parse a "character list" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *     lccl:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *     kcl:    Character list. [cl]
	 *     ncl:    Number of entries in KCL. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom, ncom
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     jcom
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lcchar, putcl
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    880923:  Original version (based on LCDFL.)
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  880923
	 *===================================================================== */
	/* PROCEDURE: */
	nerr = 0;

	/* - Assume the worst. */

	lccl_v = FALSE;

	/* - Initialize the character list and counter. */

	memset ( kcl , ' ' , kcl_s - 1 );
	*ncl = 0;

	/* - Loop until tokens are exhausted. */

L_2000:
	if( cmcom.jcom <= cmcom.ncom ){

		/* -- Get next token and store it in the character list. */
		if( lcchar( MCPFN, ktoken,MCPFN+1, &ncname ) ){
			putcl( kcl,kcl_s, ktoken,MCPFN+1, &nerr );
			if( nerr != 0 )
				goto L_8888;
			lccl_v = TRUE;
			*ncl = *ncl + 1;

			/* -- Raise error condition due to an unexpected token. */
			}
		else{
			nerr = 1001;
			goto L_8888;
			}

		/* -- Loop until command is exhausted. */
		goto L_2000;

		}

L_8888:
	return( lccl_v );

} /* end of function */

