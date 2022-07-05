#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lctok(ktok, ktok_s, lnumbr, rnumbr)
char *ktok;   int ktok_s;
int *lnumbr;
float *rnumbr;
{
	int lctok_v;
	int j, n;



	/*=====================================================================
	 * PURPOSE:  To return next token and its numerical equivalent.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ktok:    Next token in command. [k]
	 *    lnumbr:  .TRUE. if KTOK represents a real or integer number. [l]
	 *    rnumbr:  Real numerical equivalent of KTOK if LNUMBR is .TRUE.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lctok:   Set to .TRUE. if a token existed.
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom, ncom, kcom, itypcm, inumbr, ialpha, flnum
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * Unlike the other command construct functions, LCTOK does NOT
	 * increment the token counter.  This is because the token is not
	 * "parsed" until it has been evaluated.  The user of LCTOK must
	 * explicitely increment the token counter by calling ICTOK with an
	 * argument of "+1".
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820420:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	j = cmcom.jcom;
	n = cmcom.ncom;
	lctok_v = cmcom.jcom <= cmcom.ncom && cmcom.ncerr == 0;

	if( lctok_v ){
                fstrncpy(ktok,ktok_s-1,kmcom.kcom[cmcom.jcom - 1],
                                strlen(kmcom.kcom[cmcom.jcom - 1]));
		*lnumbr = Itypcm[cmcom.jcom] == cmcom.inumbr;
		if( *lnumbr )
			*rnumbr = Flnum[cmcom.jcom];
		}

L_8888:
	return( lctok_v );

} /* end of function */

