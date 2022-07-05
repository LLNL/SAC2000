#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ getstringargs(kfunction, kfunction_s, nc, ic, maxargs, 
	 icstart, icstop, nargs, nerr)
char *kfunction;   int kfunction_s;
int nc, *ic, maxargs, icstart[], icstop[], *nargs, *nerr;
{
	int itype, jargs, jargs_, notused;

	int *const Icstart = &icstart[0] - 1;
	int *const Icstop = &icstop[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To get pointers to the string arguments of an inline function.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kfunction:  Function to process. [c]
	 *    nc:         Number of characters in function. [i]
	 *    ic:         Pointer to "current" character in function. [i]
	 *    maxargs:    Maximum number of arguments to get. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    icstart:    Array of starting pointers to arguments. [ia]
	 *    icstop:     Array of stopping pointers to arguments. [ia]
	 *    nargs:      Number of arguments found. [i]
	 *    nerr:       Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:   cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:     poptok, setmsg, apimsg, aplmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890623:  Fixed bug involving do loop control variable.
	 *    890112:  Changed to allow variable number of arguments.
	 *    881229:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881229
	 *===================================================================== */
	/* - For each argument (itype=1 or 3), pop token, and save character pointers. */
	/* - End when maxargs has been reached or tokens exhausted (itype=0). */
	/* - Raise error condition if a "delimiter" was found (itype=2). */
	/* - If it is a quoted string (itype=3), increment the starting pointer by 1
	 *   so that it points to the string not the quotation mark.
	 *   The stopping pointer points to the end of the string not the closing
	 *   quotation mark, so it does not have to be adjusted. */
	for( jargs = 1; jargs <= maxargs; jargs++ ){
		jargs_ = jargs - 1;
		poptok( kfunction, nc, ic, &Icstart[jargs], &Icstop[jargs], 
		 &itype );
		if( itype == 0 ){
			*nargs = jargs - 1;
			goto L_8888;
			}
		else if( itype == 2 ){
			*nerr = 1023;
			setmsg( "ERROR", *nerr );
			aplmsg( kfunction,kfunction_s );
			*nargs = jargs;
			goto L_8888;
			}
		else if( itype == 3 ){
			Icstart[jargs] = Icstart[jargs] + 1;
			}
		}

	/* - If there are any more tokens, raise error condition. */

	poptok( kfunction, nc, ic, &notused, &notused, &itype );
	if( itype != 0 ){
		*nerr = 1026;
		setmsg( "ERROR", *nerr );
		apimsg( maxargs );
		aplmsg( kfunction,kfunction_s );
		*nargs = maxargs;
		goto L_8888;
		}

L_8888:
	return;

} /* end of function */

