#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/exm.h"
#include "../../inc/cnv.h"
void /*FUNCTION*/ xeval(nerr)
int *nerr;
{
	char ktemp[MCMSG+1], ktevaln[MCMSG+1], kvalue[MCMSG+1];
	int lvalue, lwantv;
	int ifoper, iloper, jeval, nchar;
	float value;


	/*=====================================================================
	 * PURPOSE: To parse and execute the action command EVALUATE.
	 *          This command evaluates simple arithmetic expressions.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    EXM:     KFEVAL, NEVAL, KLEVAL, NLEVAL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    EXM:     KEVALN, EVALPA, LFEVAL, IFEVAL, ILEVAL
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LKCHAR, LCREAL, LCLIST, SETBBV
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920330:  Added global icnver, set bbvar value to NaN.
	 *    920320:  Made kevaln length mcmsg. Previous defaulted to 8.
	 *    880404:  Added option to format results in integer format.
	 *    880326:  Fixed bug in computing neval and increased precision
	 *             of output floating point values.
	 *    870724:  Modified to use blackboard rather than global variable.
	 *    840822:  Defaulted first variable to 1.0 if omitted.
	 *    840615:  Original version (from FCALC program.)
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870724
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - PARSING PHASE: */

	/* - Loop on each token in command: */

	jeval = 0;
	lwantv = TRUE;
L_1000:
	if( lcmore( nerr ) ){

		/* -- "TO TERMINAL|name":  set name of variable to store output in. */
		if( lkchar( "TO#$",5, MCMSG, kmexm.kevaln, MCMSG+1, &nchar ) ){

			/* -- "AS INTEGER|FLOAT":  define output format. */
			}
		else if( lklog2( "AS#$",5, "FLOAT$",7, "INTEGER$",9, &cmexm.lfloat ) ){

			/* -- "ADD|SUB|DIV...":  define floating point operator. */
			}
		else if( lclist( (char*)kmexm.kfeval,9, cmexm.nfeval, &ifoper ) ){
			if( !lwantv ){
				if( jeval < MEVAL ){
					jeval = jeval + 1;
					Ifeval[jeval] = ifoper;
					cmexm.lfeval = TRUE;
					lwantv = TRUE;
					}
				else{
					ictok( -1 );
					cfmt( "TOO MANY VARIABLES$",20 );
					cresp();
					}
				}
			else if( jeval == 0 ){
				cmexm.feval[0] = 1.;
				jeval = 1;
				Ifeval[1] = ifoper;
				cmexm.lfeval = TRUE;
				lwantv = TRUE;
				}
			else{
				ictok( -1 );
				cfmt( "NEED A VARIABLE$",17 );
				cresp();
				}

			/* -- "LT|LE|GT...":  define logical operator. */
			}
		else if( lclist( (char*)kmexm.kleval,9, cmexm.nleval, &iloper ) ){
			if( !lwantv ){
				cmexm.ileval = iloper;
				cmexm.lfeval = FALSE;
				jeval = 1;
				lwantv = TRUE;
				}
			else{
				ictok( -1 );
				cfmt( "NEED A VARIABLE$",17 );
				cresp();
				}

			/* -- "v":  set the value of one of the operands. */
			}
		else if( lcreal( &value ) ){
			if( lwantv ){
				cmexm.feval[jeval] = value;
				lwantv = FALSE;
				}
			else{
				ictok( -1 );
				cfmt( "NEED AN OPERAND$",17 );
				cresp();
				}

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;

		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
		goto L_8888;

	/* - Update count of operations. */

	cmexm.neval = jeval;

	/* EXECUTION PHASE: */

	/* - Evaluate floating point expressions here. */

	if( cmexm.lfeval ){

		/* -- Set temporary variable to initial operand. */
		value = cmexm.feval[0];

		/* -- Perform specific calculation for each operator/operand set: */
		jeval = 1;
L_6000:
		if( jeval <= cmexm.neval ){

			switch( Ifeval[jeval] ){
				case 1: goto L_6010;
				case 2: goto L_6020;
				case 3: goto L_6030;
				case 4: goto L_6040;
				case 5: goto L_6050;
				case 6: goto L_6060;
				case 7: goto L_6070;
				case 8: goto L_6080;
				case 9: goto L_6090;
				case 10: goto L_6100;
				case 11: goto L_6110;
				case 12: goto L_6160;
				case 13: goto L_6130;
				case 14: goto L_6140;
				case 15: goto L_6150;
				case 16: goto L_6010;
				case 17: goto L_6020;
				case 18: goto L_6030;
				case 19: goto L_6040;
				case 20: goto L_6050;
				}

			/* --- Addition. */
L_6010:
			value = value + cmexm.feval[jeval];
			goto L_6999;
			/* --- Subtraction. */
L_6020:
			value = value - cmexm.feval[jeval];
			goto L_6999;
			/* --- Multiplication. */
L_6030:
			value = value*cmexm.feval[jeval];
			goto L_6999;
			/* --- Division with error checking for overflow. */
L_6040:
			if( cmexm.feval[jeval] != 0. ){
				value = value/cmexm.feval[jeval];
				}
			else{
				*nerr = 1117;
				}
			goto L_6999;
			/* -- Power. */
L_6050:
			value = pow(value,cmexm.feval[jeval]);
			goto L_6999;
			/* --- Square root with error checking. */
L_6060:
			if( cmexm.feval[jeval] >= 0. ){
				value = value*sqrt( cmexm.feval[jeval] );
				}
			else{
				*nerr = 1117;
				}
			goto L_6999;
			/* --- Exponentiation. */
L_6070:
			value = value*exp( cmexm.feval[jeval] );
			goto L_6999;
			/* --- Natural logarithm. */
L_6080:
			if( cmexm.feval[jeval] > 0. ){
				value = value*log( cmexm.feval[jeval] );
				}
			else{
				*nerr = 1117;
				}
			goto L_6999;
			/* --- Base 10 logarithm. */
L_6090:
			if( cmexm.feval[jeval] > 0. ){
				value = value*log10( cmexm.feval[jeval] );
				}
			else{
				*nerr = 1117;
				}
			goto L_6999;
			/* --- Sine. */
L_6100:
			value = value*sin( cmexm.feval[jeval] );
			goto L_6999;
			/* --- Arcsine. */
L_6110:
			if( cmexm.feval[jeval] <= 1. ){
				value = value*asin( cmexm.feval[jeval] );
				}
			else{
				*nerr = 1117;
				}
			goto L_6999;
			/* --- Cosine. */
L_6160:
			value = value*cos( cmexm.feval[jeval] );
			goto L_6999;
			/* --- Arccosine. */
L_6130:
			if( cmexm.feval[jeval] <= 1. ){
				value = value*acos( cmexm.feval[jeval] );
				}
			else{
				*nerr = 1117;
				}
			goto L_6999;
			/* --- Tangent. */
L_6140:
			value = value*tan( cmexm.feval[jeval] );
			goto L_6999;
			/* --- Arctangent. */
L_6150:
			value = value*atan( cmexm.feval[jeval] );
			goto L_6999;

			/* --- Check to see if an error has occurred during this pass.
			 *     Check for conversion WARNING first - then ERROR second. */
L_6999:
			if( *nerr != 0 ){
				setmsg( "ERROR", *nerr );
				apfmsg( cmexm.feval[jeval] );
				goto L_8888;
				}

			/* -- Loop until entire expression has been evaluated. */
			jeval = jeval + 1;
			goto L_6000;
			}

		/* -- Encode evaluated expression. */
		if( cmexm.lfloat ){
                        sprintf(kvalue,"%16.7e",value);
			}
		else{
                        sprintf(kvalue,"%16d", (int)( value + VSMALL ) );
			}
		ljust( kvalue,MCMSG+1 );

		}
	else{

		/* - Evaluate logical expression. */

		switch( cmexm.ileval ){
			case 1: goto L_7010;
			case 2: goto L_7020;
			case 3: goto L_7030;
			case 4: goto L_7040;
			case 5: goto L_7050;
			case 6: goto L_7060;
			}

L_7010:
		lvalue = cmexm.feval[0] < cmexm.feval[1];
		goto L_7999;

L_7020:
		lvalue = cmexm.feval[0] <= cmexm.feval[1];
		goto L_7999;

L_7030:
		lvalue = cmexm.feval[0] > cmexm.feval[1];
		goto L_7999;

L_7040:
		lvalue = cmexm.feval[0] >= cmexm.feval[1];
		goto L_7999;

L_7050:
		lvalue = cmexm.feval[0] == cmexm.feval[1];
		goto L_7999;

L_7060:
		lvalue = cmexm.feval[0] != cmexm.feval[1];
		goto L_7999;

L_7999:
		if( lvalue ){
			fstrncpy( kvalue, MCMSG, "TRUE", 4 );
			}
		else{
			fstrncpy( kvalue, MCMSG, "FALSE", 5 );
			}

		}

	/* - Write value to either the message subsystem or to a global variable. */

	/* - Test for conversion error (from cnvati, cnvatf, etc). If the answer 
	 *   could be wrong, set it to not a number (NaN). */

	if( cmicnv.icnver != 0 ){
		fstrncpy( kvalue, MCMSG, "NaN", 3);
		cmicnv.icnver = 0;
		}

	ljust( kvalue,MCMSG+1 );
	modcase( TRUE, kmexm.kevaln, MCMSG+1, ktemp );
	if( memcmp(ktemp,"TERM",4) == 0 ){
		setmsg( "OUTPUT", 99 );
		apcmsg( kvalue,MCMSG+1 );
		outmsg();
		clrmsg();
		}
	else{
		setbbv( kmexm.kevaln, kvalue, nerr, MCMSG, MCMSG );
		}

L_8888:
	return;
} /* end of function */

