#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MOK	12

void /*FUNCTION*/ cnvatf(kfloat, kfloat_s, floatn, lstrict, nerr)
char *kfloat;   int kfloat_s;
float *floatn;
int lstrict, *nerr;	/* lstrict added. maf 970129 */
{
	char kexp[13], kfract[13], kwhnum[13];
	int idx, ipart, jch, jch1, jch2, jok, ncf, nexp, nfract, 
	 nwhnum;
	float exp, fract, sign, whnum;
	static byte kok[MOK]={'0','1','2','3','4','5','6','7','8','9',
	 '+','-'};
	static int iwhnum = 1;
	static int ifract = 2;
	static int iexp = 3;
	static int idone = 4;

	byte *const Kok = &kok[0] - 1;


	/*=====================================================================
	 * PURPOSE: To convert an ASCII symbol to its floating point equivalent.
	 *          An error flag is set if the symbol is alphanumeric.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KFLOAT:  ASCII symbol (left justified).
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    FLOATN:   Floating point equivalent.
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Set to 1 if KFLOAT is alphanumeric.
	 *             Set to 901 if a logic error was detected.
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  CNVATI
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * LIMITATIONS:
	 *=====================================================================
	 * KNOWN BUGS:
	 * - Would pass "12-34" onto CNVATI as a single part of number.
	 *   Need to change logic involving "+" and "-" characters [820924].
	 * - The entire loop could use some housecleaning [820924].
	 *=====================================================================
	 * EXTERNAL DEPENDENCIES:
	 * - Fortran 77
	 *===================================================================== */
	/* PROCEDURE: */
	/*   Uses Fortran 77 character substrings to scan input symbol and
	 *   pull off the sign, whole number, fraction, and exponent separately.
	 *   If any of the parts are missing, default values are used.
	 *   They are then combined into the floating point number. */
	/*   Scan is stopped when NCF characters have been evaulated or
	 *   when an illegal or blank character is encountered.
	 *   A symbol containing only blanks is considered alphanumeric.
	 *   Leading blanks are ignored. */
	/* - Initialize the various parts of the floating point number. */
	*nerr = 0;
	sign = 1.;
	whnum = 0.;
	fract = 0.;
	exp = 0.;
	ipart = iwhnum;
	jch = 1;
        for( idx = 0 ; idx < 12 ; idx++ ) {
	    kwhnum[ idx ] = ' ' ;
	    kfract[ idx ] = ' ' ;
            kexp[ idx ]   = ' ' ;
        }
        kwhnum[12] = '\0' ;
        kfract[12] = '\0' ;
	kexp[ 12 ] = '\0' ;

	/* - Determine length of character string. */

	ncf = (kfloat_s - 1);

	/* - Skip any leading blanks.
	 *   Take error return if symbol contains only blanks. */

	while( kfloat[jch - 1] == ' ' ){
		jch++ ;
		if( jch > ncf ){
			*nerr = 1;
			goto L_8888;
		}
	}

	/* - Check for leading "+" or "-" */

	if( kfloat[jch - 1] == '-' ){
		sign = -1.;
		jch = jch + 1;
	}
	else if( kfloat[jch - 1] == '+' ){
		sign = 1.;
		jch = jch + 1;
	}

	/* - Main loop on each character.
	 *   JCH points to current character.
	 *   JCH1 points to first character of current "part" of number.
	 *   JCH2 points to last character of current "part". */

	*nerr = 0;
	jch1 = jch;

	/* - Terminate when a blank is encountered or we are out of characters. */

L_1000:
	if( kfloat[jch - 1] == ' ' || jch > ncf ){
		if( ipart == iwhnum ){
			jch2 = jch - 1;
			fstrncpy( kwhnum, 12, kfloat+jch1 - 1,jch2 - jch1 + 1);
						/* lstrict added. maf 970129 */
			cnvati( kwhnum,13, &nwhnum, lstrict, nerr );
			if( *nerr == 0 ){
				whnum = nwhnum;
				ipart = ifract;
				jch1 = jch + 1;
			}
			else{
				*nerr = 4;
				goto L_8888;
			}
		}
		else if( ipart == ifract ){
			jch2 = jch - 1;
			fstrncpy( kfract, 12, kfloat+jch1 - 1,jch2 - jch1 + 1);
						/* lstrict added. maf 970129 */
			cnvati( kfract,13, &nfract, lstrict, nerr );
			if( *nerr == 0 ){
				fract = nfract/powi(10.,jch2 - jch1 + 1);
				ipart = iexp;
				jch1 = jch + 1;
			}
			else{
				*nerr = 5;
				goto L_8888;
			}
		}
		else if( ipart == iexp ){
			jch2 = jch - 1;
			fstrncpy( kexp, 12, kfloat+jch1 - 1,jch2 - jch1 + 1);
						/* lstrict added. maf 970129 */
			cnvati( kexp,13, &nexp, lstrict, nerr );
			if( *nerr == 0 ){
				exp = nexp;
				ipart = idone;
				jch1 = jch + 1;
			}
			else{
				*nerr = 6;
				goto L_8888;
			}
		}
		else{
			*nerr = 1;
			goto L_8888;
		}
		*floatn = sign*(whnum + fract);
		if( exp != 0. )
			*floatn = *floatn*pow(10.,exp);
		*nerr = 0;
		goto L_8888;

		/* - A "." separates the whole number from the fraction. */

	}
	else if( kfloat[jch - 1] == '.' ){
		if( ipart == iwhnum ){
			jch2 = jch - 1;
			fstrncpy( kwhnum, 12, kfloat+jch1 - 1,jch2 - jch1 + 1);
						/* lstrict added. maf 970129 */
			cnvati( kwhnum,13, &nwhnum, lstrict, nerr );
			if( *nerr == 0 ){
				whnum = nwhnum;
				ipart = ifract;
				jch1 = jch + 1;
			}
			else{
				*nerr = 2;
				goto L_8888;
			}
		}
		else{
			*nerr = 1;
			goto L_8888;
		}

		/* - An "E" separates the fraction from the exponent. */

	}
	else if( kfloat[jch - 1] == 'E' || kfloat[jch - 1] == 'e' ){
		if( jch == 1 ){
			*nerr = 1;
			goto L_8888;
		}
		else if( ipart == iwhnum ){
			jch2 = jch - 1;
			fstrncpy( kwhnum, 12, kfloat+jch1 - 1,jch2 - jch1 + 1);
						/* lstrict added. maf 970129 */
			cnvati( kwhnum,13, &nwhnum, lstrict, nerr );
			if( *nerr == 0 ){
				whnum = nwhnum;
			}
			else{
				*nerr = 7;
				goto L_8888;
			}
		}
		else if( ipart == ifract ){
			jch2 = jch - 1;
			fstrncpy( kfract, 12, kfloat+jch1 - 1,jch2 - jch1 + 1);
						/* lstrict added. maf 970129 */
			cnvati( kfract,13, &nfract, lstrict, nerr );
			if( *nerr == 0 ){
				fract = nfract/powi(10.,jch2 - jch1 + 1);
			}
			else{
				*nerr = 3;
				goto L_8888;
			}
		}
		else{
			*nerr = 1;
			goto L_8888;
		}
		ipart = iexp;
		jch1 = jch + 1;

		/* - Check for a legitimate character ("0"-"9","+","-"). */

	}
	else{
		jok = 1;
L_4000:
		if( kfloat[jch - 1] != Kok[jok] ){
			if( jok < MOK ){
				jok = jok + 1;
				goto L_4000;
			}
			else{
				*nerr = 1;
				goto L_8888;
			}
		}
	}

	/* - Increment character index and loop. */

	jch = jch + 1;
	goto L_1000;

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
         *    970129:  Added lstrict to indicate how to deal with errors of
         *             a string of digits that is to int to be converted to
         *             an int.  1 means catch the error and warn the user;
         *             0 means let it slide unnoticed.  maf 
	 *    830824:  Fixed bug involving symbols containing only blanks.
	 *    820924:  Fixed bug involving leading blanks.
	 *    820304:  Added logic for variable length input symbol.
	 *    810212:  Changed to output message retrieval from disk.
	 *===================================================================== */

} /* end of function */

