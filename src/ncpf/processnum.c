#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MARGS	50

#include "../../inc/mach.h"
#include "../../inc/cpf.h"
void /*FUNCTION*/ processnumeric(kfunction, kfunction_s, nc, ic, index, 
	 kvalue, kvalue_s, nerr)
char *kfunction;   int kfunction_s;
int nc, *ic, index;
char *kvalue;   int kvalue_s;
int *nerr;
{
	int lfloat;
	int j, j_, nargs;
	float args[MARGS], value;

	float *const Args = &args[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To "process" an inline numeric function from a SAC command line.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kfunction:   Body of function before expansion. [c]
	 *    nc:          Number of characters in function. [i]
	 *    ic:          Pointer to current character in function. [i]
	 *    index:       Index number of the numeric function to process. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    kvalue:      Value of function after expansion. [c]
	 *    nerr:        Error return flag. [i]
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    PI
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getnumericalargs, setmsg, apcmsg, apimsg
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890112:  Added MIN, MAX, and ABS functions.
	 *    881228:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881228
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Assume result will be real (floating point.) */

	lfloat = TRUE;

	/* - Get the numeric values (arguments) for this inline function. */

	getnumericargs( kfunction,kfunction_s, nc, ic, MARGS, args, &nargs, 
	 nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Raise error condition if the number of arguments is incorrect. */

	if( Nnumericargs[index] >= 0 && Nnumericargs[index] != nargs ){
		*nerr = 1021;
		setmsg( "ERROR", *nerr );
		apimsg( Nnumericargs[index] );
		aplmsg( kfunction,kfunction_s );
		goto L_8888;
		}

	/* - Based on the index number, jump to the coding for this function. */

	switch( index ){
		case 1: goto L_1010;
		case 2: goto L_1020;
		case 3: goto L_1030;
		case 4: goto L_1040;
		case 5: goto L_1050;
		case 6: goto L_1060;
		case 7: goto L_1070;
		case 8: goto L_1080;
		case 9: goto L_1090;
		case 10: goto L_1100;
		case 11: goto L_1110;
		case 12: goto L_1120;
		case 13: goto L_1130;
		case 14: goto L_1140;
		case 15: goto L_1150;
		case 16: goto L_1160;
		case 17: goto L_1170;
		case 18: goto L_1180;
		case 19: goto L_1190;
		case 20: goto L_1200;
		}

	/*  (This coding should never be executed.) */

	*nerr = 901;
	setmsg( "ERROR", *nerr );
	apcmsg( "in processnumeric:",19 );
	apimsg( index );
	goto L_8888;

	/* -- Add (sum) one or more numbers together.
	 *    Syntax: ADD number1 [number2 ...] */

L_1010:
	value = Args[1];
	for( j = 2; j <= nargs; j++ ){
		j_ = j - 1;
		value = value + Args[j];
		}
	goto L_8000;

	/* -- Subtract a set of numbers.
	 *    Syntax: SUBTRACT number1 [number2 ...] */

L_1020:
	value = Args[1];
	for( j = 2; j <= nargs; j++ ){
		j_ = j - 1;
		value = value - Args[j];
		}
	goto L_8000;

	/* -- Multiply a set of numbers.
	 *    Syntax: MULTIPLY number1 [number2 ...] */

L_1030:
	value = Args[1];
	for( j = 2; j <= nargs; j++ ){
		j_ = j - 1;
		value = value*Args[j];
		}
	goto L_8000;

	/* -- Divide a set of numbers.
	 *    Syntax: DIVIDE number1 [number2 ...] */

L_1040:
	value = Args[1];
	for( j = 2; j <= nargs; j++ ){
		j_ = j - 1;
		if( Args[j] != 0.0 ){
			value = value/Args[j];
			}
		else{
			*nerr = 1022;
			setmsg( "ERROR", *nerr );
			apcmsg( "division by zero.",18 );
			aplmsg( kfunction,kfunction_s );
			goto L_8888;
			}
		}
	goto L_8000;

	/* -- Take square root of a number.
	 *    Syntax: SQRT number */
L_1050:
	if( Args[1] > 0.0 ){
		value = sqrt( Args[1] );
		}
	else{
		*nerr = 1022;
		setmsg( "ERROR", *nerr );
		apcmsg( "square root of negative number.",32 );
		aplmsg( kfunction,kfunction_s );
		goto L_8888;
		}
	goto L_8000;

	/* -- Exponentiate a number.
	 *    Syntax: EXP number */
L_1060:
	value = exp( Args[1] );
	goto L_8000;

	/* -- Take natural log of a number.
	 *    Syntax: ALOG number */
L_1070:
	value = log( Args[1] );
	goto L_8000;

	/* -- Raise a number to power of ten.
	 *    Syntax: POWER number */
L_1080:
	value = pow(10.0,Args[1]);
	goto L_8000;

	/* -- Take log to base 10 of a number.
	 *    Syntax: ALOG10 number */
L_1090:
	value = log10( Args[1] );
	goto L_8000;

	/* -- Take sine of a number.
	 *    Syntax: SIN number */
L_1100:
	value = sin( Args[1] );
	goto L_8000;

	/* -- Take arcsine of a number.
	 *    Syntax: ASIN number */
L_1110:
	value = asin( Args[1] );
	goto L_8000;

	/* -- Take cosine of a number.
	 *    Syntax: COS number */
L_1120:
	value = cos( Args[1] );
	goto L_8000;

	/* -- Take arccosine of a number.
	 *    Syntax: ACOS number */
L_1130:
	value = acos( Args[1] );
	goto L_8000;

	/* -- Take tangent of a number.
	 *    Syntax: TAN number */
L_1140:
	value = tan( Args[1] );
	goto L_8000;

	/* -- Take arctangent of a number.
	 *    Syntax: ATAN number */
L_1150:
	value = atan( Args[1] );
	goto L_8000;

	/* -- Convert argument to an integer.
	 *    Syntax: INT number */
L_1160:
	value = Args[1];
	lfloat = FALSE;
	goto L_8000;

	/* -- Set value to pi.
	 *    Syntax: PI */
L_1170:
	value = PI;
	goto L_8000;

	/* -- Compute the minimum value of a set of numbers.
	 *    Syntax: MIN number1 [number2 ...] */

L_1180:
	value = Args[1];
	for( j = 2; j <= nargs; j++ ){
		j_ = j - 1;
		value = fmin( value, Args[j] );
		}
	goto L_8000;

	/* -- Compute the minimum value of a set of numbers.
	 *    Syntax: MAX number1 [number2 ...] */

L_1190:
	value = Args[1];
	for( j = 2; j <= nargs; j++ ){
		j_ = j - 1;
		value = fmax( value, Args[j] );
		}
	goto L_8000;

	/* -- Take the absolute value of a number.
	 *    Syntax: ABS number */
L_1200:
	value = fabs( Args[1] );
	goto L_8000;

	/* - Encode resulting real or integer as a character string and left justify it. */

L_8000:
	if( lfloat ){
                sprintf(kvalue,"%16.7E",value);
		}
	else{
                sprintf(kvalue,"%16d", (int)( value + VSMALL ) );
		}
	ljust( kvalue,kvalue_s );

L_8888:
	return;

} /* end of function */

