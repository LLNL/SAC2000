#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#define	MLOGICALS	6

#include "../../inc/mach.h"
void /*FUNCTION*/ evallogical(string, string_s, result, result_s)
char *string;   int string_s;
char *result;   int result_s;
{
	char test[3];
	int lfloat1, lfloat3, lresult;
	int ic, ic1b, ic1e, ic2b, ic2e, ic3b, ic3e, ilogical, itype, 
	 nc, nerr;
	float expr1, expr3, x, y;
	void *_p0;
        char *s1;
	static char logicals[MLOGICALS][3]={"LT","LE","GT","GE","EQ","NE"};

	/*=====================================================================
	 * PURPOSE: To parse and evaluate a "logical" expression.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    string:  Character string containing logical expression. [c]
	 *             Must be of the form: "number logical number" where
	 *             number is an integer or floating point number and
	 *             logical is one of: 'LT', 'LE', 'GT', 'GE', 'EQ', 'NE'.
	 *             Integers are converted to floating before being evaluated.
	 *             Leading, trailing, and extra white space is ignored.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    result:  Character string containing the result. [c]
	 *             = 'TRUE' if the expression is true.
	 *             = 'FALSE' if the expression is false.
	 *             = 'ERROR' if an error occurred during parsing.
	 *=====================================================================
	 * MODULE/LEVEL:  UCF/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    VSMALL, RNDOFF
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  indexb, poptok, cnvatf, modcase, lequal
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *    970129:  Add parameter (0) to cnvatf.  0 means that if a string
         *             of digits is too int, let it slide by.  maf 
	 *    900606:  Modified tests for reals to handle case where
	 *             both numbers are zero.
	 *    871117:  Changed tests of (in)equality of reals to include
	 *             the possible effects of roundoff.
	 *             These are now tests for equivalence not equality.
	 *    871103:  Added ability to check for (in)equality of strings.
	 *    870811:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  871103
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Pop first token from input character string and convert it to
	 *   a floating point number. */
	nc = indexb( string,string_s );
	ic = 0;

	poptok( string, nc, &ic, &ic1b, &ic1e, &itype );
	if( itype != 1 ){
		fstrncpy( result, result_s-1, "ERROR", 5 );
		goto L_8888;
		}

        strncpy((s1=malloc(ic1e - ic1b + 2)),string+ic1b - 1,ic1e - ic1b + 1);
        s1[ic1e - ic1b + 1] = '\0';

	cnvatf( s1 ,ic1e - ic1b + 2, &expr1, 0, &nerr );
					/* add 0, maf 970129 */

	free(s1);

	lfloat1 = nerr == 0;

	/* - Pop second token, convert to upper case, 
	 *   and make sure it is a valid logical token. */

	poptok( string, nc, &ic, &ic2b, &ic2e, &itype );
       
        strncpy((s1=malloc(ic2e - ic2b + 2)),string+ic2b - 1,ic2e - ic2b + 1);
        s1[ic2e - ic2b + 1] = '\0';

	modcase( TRUE, s1, 2, test );

        free(s1);

	if( !lequal( test,3, (char*)logicals,3, MLOGICALS, &ilogical )
	  ){
		fstrncpy( result, result_s-1, "ERROR", 5 );
		goto L_8888;
		}

	/* - Pop third token and convert to floating point number. */

	poptok( string, nc, &ic, &ic3b, &ic3e, &itype );
	if( itype != 1 ){
		fstrncpy( result, result_s-1, "ERROR", 5 );
		goto L_8888;
		}

        strncpy((s1=malloc(ic3e - ic3b + 2)),string+ic3b - 1,ic3e - ic3b + 1);
        s1[ic3e - ic3b + 1] = '\0';

	cnvatf( s1 ,ic3e - ic3b + 2, &expr3, 0, &nerr );
					/* add 0, maf 970129 */

	free(s1);
	lfloat3 = nerr == 0;

	/* - If first and third tokens are not floating point numbers (i.e. strings)
	 *   then the only tests that are allowed are "EQ" and "NE". */

	if( !lfloat1 || !lfloat3 ){
		if( ilogical == 5 ){
			lresult = memcmp(string+ic1b - 1,string+ic3b - 1
                                        ,max(ic1e-ic1b+1,ic3e-ic3b+1)) == 0;
			goto L_8000;
			}
		else if( ilogical == 6 ){
			lresult = memcmp(string+ic1b - 1,string+ic3b - 1
                                         ,max(ic1e-ic1b+1,ic3e-ic3b+1)) != 0;
			goto L_8000;
			}
		else{
			fstrncpy( result, result_s-1, "ERROR", 5 );
			goto L_8888;
			}
		}

	/* - Evaluate logical expressions involving floating point numbers. */

	switch( ilogical ){
		case 1: goto L_1010;
		case 2: goto L_1020;
		case 3: goto L_1030;
		case 4: goto L_1040;
		case 5: goto L_1050;
		case 6: goto L_1060;
		}

L_1010:
	lresult = expr1 < expr3;
	goto L_8000;

L_1020:
	lresult = expr1 <= expr3;
	goto L_8000;

L_1030:
	lresult = expr1 > expr3;
	goto L_8000;

L_1040:
	lresult = expr1 >= expr3;
	goto L_8000;

L_1050:
	y = fabs( expr1 - expr3 );
	x = fmax( 0.5*(expr1 + expr3), VSMALL );
	lresult = (y/x) <= RNDOFF;
	goto L_8000;

L_1060:
	y = fabs( expr1 - expr3 );
	x = fmax( 0.5*(expr1 + expr3), VSMALL );
	lresult = (y/x) > RNDOFF;
	goto L_8000;

	/* - Encode result of logical test. */

L_8000:
	if( lresult ){
		fstrncpy( result, result_s - 1, "TRUE", 4 );
		}
	else{
		fstrncpy( result, result_s - 1, "FALSE", 5 );
		}

L_8888:
	return;

} /* end of function */

