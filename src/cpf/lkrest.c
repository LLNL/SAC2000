#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lkrest(kkey, kkey_s, mchar, kchar, kchar_s, 
	 nchar)
char *kkey;   int kkey_s;
int mchar;
char *kchar;   int kchar_s;
int *nchar;
{
	int lkrest_v;
	int ic1;



	/*=====================================================================
	 * PURPOSE:  To parse the "keyed rest of command line."
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lkrest:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *    mchar:   Maxiumum length of KCHAR. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    kchar:   Output character string. [c]
	 *    nchar:   Length of KCHAR. [i]
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
	 *    820914:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900510
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Search for key.  Return if not found. */
	lkrest_v = lckey( kkey,kkey_s );
	if( !lkrest_v )
		goto L_8888;

	*nchar = 0;
	ic1 = 1;
L_1000:
	if( cmcom.jcom <= cmcom.ncom ){
		*nchar = min( *nchar + MCPW, mchar );
		subscpy( kchar, ic1 - 1, *nchar - 1, kchar_s - 1, kmcom.kcom[cmcom.jcom - 1]
		  );
		cmcom.jcom = cmcom.jcom + 1;
		ic1 = *nchar + 1;
		goto L_1000;
		}

L_8888:
	return( lkrest_v );

} /* end of function */

