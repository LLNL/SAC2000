#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/* APPEND -- SUBROUTINE TO APPEND ONE CHARACTER STRING TO ANOTHER
 *
 *  DAVE HARRIS
 *
 *  LAST MODIFIED:  JULY 24, 1981
 * */
void /*FUNCTION*/ append(line, line_s, exp, exp_s)
char *line;   int line_s;
char *exp;   int exp_s;
{
	byte delim;
	int iptr, jptr;
        char *strtemp;


	/*  LOCATE END OF LINE
	 * */
	iptr = (line_s - 1);
L_1:
	;
	if( line[iptr - 1] != ' ' ){
		goto L_2;
		}
	else{
		iptr = iptr - 1;
		if( iptr < 1 ){
			return;
			}
		}
	goto L_1;
L_2:
	;

	/*  OBTAIN DELIMITER ( FIRST CHARACTER OF EXP )
	 * */
	delim = exp[0];

	/*  APPEND EXPRESSION IF NOT NULL
	 * */
	if( exp[1] != delim ){

		/*    LOCATE END OF EXPRESSION
		 * */
		jptr = 2;
L_3:
		;
		if( exp[jptr] == delim ){
			goto L_4;
			}
		else{
			jptr = jptr + 1;
			if( jptr > (exp_s - 1) ){
				return;
				}
			}
		goto L_3;
L_4:
		;

		/*    APPEND EXPRESSION TO LINE
		 * */
                strtemp = malloc(jptr);
                strncpy(strtemp,exp+1,jptr-1);
                strtemp[jptr-1] = '\0';
		subscpy( line, iptr, (line_s - 1) - 1, line_s - 1, strtemp);
                free(strtemp);

		}

	/*  DONE
	 * */
	return;
} /* end of function */

