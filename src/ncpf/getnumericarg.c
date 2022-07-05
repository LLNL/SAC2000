#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ getnumericargs(kfunction, kfunction_s, nc, ic, maxargs, 
	 args, nargs, nerr)
char *kfunction;   int kfunction_s;
int nc, *ic, maxargs;
float args[];
int *nargs, *nerr;
{
	int ic1, ic2, itype, jargs, jargs_, notused;
	void *_p0;
        char *s1;


	float *const Args = &args[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To get the numerical arguments of an inline function.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kfunction:  Function to process. [c]
	 *    nc:         Number of characters in function. [i]
	 *    ic:         Pointer to "current" character in function. [i]
	 *    maxargs:    Maximum number of numeric arguments to get. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    args:       Array of numeric arguments. [ra]
	 *    nargs:      Number of numeric arguments returned. [i]
	 *    nerr:       Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:   cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:     poptok, cnvatf, setmsg, apimsg, apcmsg, aplmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *    970129:  Add parameter (0) to cnvatf.  0 means that if a string
         *             of digits is too int, let it slide by.  maf 
	 *    890623:  Fixed bug involving do loop control variable.
	 *    881229:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881229
	 *===================================================================== */
	/* - For each argument (itype=1 or 3), pop token, and convert to floating point. */
	/* - End when maxargs has been reached or tokens exhausted (itype=0). */
	/* - Raise error condition if value can't be converted to floating point. */
	/* - Raise error condition if a "delimiter" was found (itype=2). */
	/* - If it is a quoted string (itype=3), increment the starting pointer by 1
	 *   so that it points to the string not the quotation mark.
	 *   The stopping pointer points to the end of the string not the closing
	 *   quotation mark, so it does not have to be adjusted. */
	for( jargs = 1; jargs <= maxargs; jargs++ ){
		jargs_ = jargs - 1;
		poptok( kfunction, nc, ic, &ic1, &ic2, &itype );
		if( itype == 0 ){
			*nargs = jargs - 1;
			goto L_8888;
			}
		else if( itype == 1 ){
                        strncpy((s1=malloc(ic2-ic1+2)),kfunction+ic1 - 1,
                                           ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';

			cnvatf( s1, ic2-ic1 + 2, &Args[jargs], 0, nerr );
							/* add 0. maf 970129 */

			free(s1);

			if( *nerr != 0 ){
				*nerr = 1023;
				setmsg( "ERROR", *nerr );
				aplmsg( kfunction,kfunction_s );
				*nargs = jargs;
				goto L_8888;
				}
			}
		else if( itype == 2 ){
			*nerr = 1023;
			setmsg( "ERROR", *nerr );
			aplmsg( kfunction,kfunction_s );
			*nargs = jargs;
			goto L_8888;
			}
		else if( itype == 3 ){
			ic1 = ic1 + 1;
                        strncpy((s1=malloc(ic2-ic1+2)),kfunction+ic1 - 1,
                                           ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';

			cnvatf( s1, ic2-ic1 + 2, &Args[jargs], 0, nerr );
							/* add 0. maf 970129 */

			free(s1);

			if( *nerr != 0 ){
				*nerr = 1023;
				setmsg( "ERROR", *nerr );
				aplmsg( kfunction,kfunction_s );
				*nargs = jargs;
				goto L_8888;
				}
			}
		}

	/* - If there are any more tokens, also raise an error condition. */

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

