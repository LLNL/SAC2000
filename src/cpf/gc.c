#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/csf.h"
#include "../../inc/com.h"
void /*FUNCTION*/ gc(lexist, nerr)
int *lexist;
int *nerr;
{
	int i, j, jcom_, n, ninvok;



	/*=====================================================================
	 * PURPOSE:  To get the next command from the command stack.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    lexist:  Set to .TRUE. if a command exists on command stack. [l]
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    csf:     lcsemp
	 *    com:     MCOM
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     ncom, kcom, ncerr
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *    ctype:   Modifies (com: itypcm, flnum, ncom).
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     cspop, ctype, cspush
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820919:  Added logic to reject null commands.
	 *    820423:  Adjustments due to new command parsing system.
	 *    820310:  Will now convert alpha symbols to upper case.
	 *    820304:  Can now handle floating numbers of length > MCPW.
	 *    820122:  Made length of input message a machine parameter.
	 *    810429:  Changes needed for downloading commands from a program.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900510
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Blank last command. */

	for( cmcom.jcom = 1; cmcom.jcom <= cmcom.ncom; cmcom.jcom++ ){
		jcom_ = cmcom.jcom - 1;
		strcpy( kmcom.kcom[jcom_], "        " );
		}

        for( i = 0; i < cmcom.ncom; i++){
          cmcom.itypcm[i] = 0;
	}

	cmcom.ncerr = 0;

	/* - Return if the command stack is empty. */

L_1500:
	cmcom.ncom = 0;
	*lexist = !cmcsf.lcsemp;
	if( cmcsf.lcsemp )
		goto L_8888;

	/* - Pop a (possibly incomplete) command off the CS. */

L_2000:
	cspop( (char*)kmcom.kcom,9, MCOM, &ninvok, &cmcom.ncom, nerr );
	n = cmcom.ncom;
	if( *nerr != 0 )
		goto L_8888;

	/* - Disallow null commands. */

	if( cmcom.ncom <= 0 )
		goto L_1500;

	/* - Determine if each token is a numeric or alphanumeric. */

	j = 1;
L_5000:
	if( j <= cmcom.ncom ){
		ctype( &j );
		j = j + 1;
		goto L_5000;
		}

	/* - If NINVOK is positive, a macro invocation request has been made. The
	 *   value of NINVOK points to the word in the KCOM array that contains
	 *   the request. The macro is loaded by CSPUSH and the CS again popped. */

	if( ninvok > 0 ){
		cspush( ninvok, nerr );
		if( *nerr != 0 )
			goto L_8888;
		cmcom.ncom = ninvok - 1;
		goto L_2000;
		}

	/* - If NINVOK is non-positive, a complete command has been formed. */

	/* - Set command pointer to first location. */

	cmcom.jcom = 1;

L_8888:
	return;

} /* end of function */

