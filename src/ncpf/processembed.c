#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MARGS	50

#include "../../inc/mach.h"
#include "../../inc/cpf.h"
void /*FUNCTION*/ processembedded(kfunction, kfunction_s, nc, ic, 
	 first, kvalue, kvalue_s, nerr)
char *kfunction;   int kfunction_s;
int nc, *ic;
double first;
char *kvalue;   int kvalue_s;
int *nerr;
{
	int index, iops[MARGS], jargs, nargs;
	float args[MARGS], value;

	float *const Args = &args[0] - 1;
	int *const Iops = &iops[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To "process" an inline embedded function from a SAC command line.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kfunction:   Body of function before expansion. [c]
	 *    nc:          Number of characters in function. [i]
	 *    ic:          Pointer to current character in function. [i]
	 *    first:       First numeric value in embedded function. [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    kvalue:      Value of function after expansion. [c]
	 *    nerr:        Error return flag. [i]
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getnumericalargs, setmsg, apcmsg, apimsg
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881228:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881228
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Get the op numbers (an integer saying which numeric operation to perform)
	 *   and numeric values (arguments) for this embedded function. */

	getembeddedargs( kfunction,kfunction_s, nc, ic, MARGS, iops, args, 
	 &nargs, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - For each pair of ops and args, based on the op number, perform the operation. */

	jargs = 1;
	value = first;

L_1000:
	if( jargs <= nargs ){

		/* -- Add argument to value. */
		if( Iops[jargs] == 1 ){
			value = value + Args[jargs];

			/* -- Subtract argument from value. */
			}
		else if( Iops[jargs] == 2 ){
			value = value - Args[jargs];

			/* -- Multiply value by argument. */
			}
		else if( Iops[jargs] == 3 ){
			value = value*Args[jargs];

			/* -- Divide value by argument. */
			}
		else if( Iops[jargs] == 4 ){
			if( Args[jargs] != 0 ){
				value = value/Args[jargs];
				}
			else{
				*nerr = 1022;
				setmsg( "ERROR", *nerr );
				apcmsg( "division by zero.",18 );
				aplmsg( kfunction,kfunction_s );
				goto L_8888;
				}

			/* -- Raise value by arguemnt. */
			}
		else if( Iops[jargs] == 5 ){
			value = pow(value,Args[jargs]);

			/* -- Error condition if unknown op number. */
			}
		else{
			*nerr = 901;
			setmsg( "ERROR", *nerr );
			apcmsg( "in processembedded:",20 );
			apimsg( index );
			goto L_8888;
			}

		/* -- Increment argument counter and loop until no more arguments. */
		jargs = jargs + 1;
		goto L_1000;
		}

	/* - Encode resulting value as a character string and left justify it. */

        sprintf(kvalue,"%16.7g",value);
	ljust( kvalue,kvalue_s );

L_8888:
	return;

} /* end of function */

