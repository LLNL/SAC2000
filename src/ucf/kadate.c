#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
void /*FUNCTION*/ kadate(iyear, ijday, ncdate, kkdate, kkdate_s, nerr)
int iyear, ijday, ncdate;
char *kkdate;   int kkdate_s;
int *nerr;
{
	char _c0[2], kenc[5];
	int iday, imonth;
	static char kmonth[12][5]={"JAN ","FEB ","MAR ","APR ","MAY ",
	 "JUN ","JUL ","AUG ","SEP ","OCT ","NOV ","DEC "};
        char *cattemp;
        char *s1;

	/*=====================================================================
	 * PURPOSE: To convert two integer fields representing year and
	 *          julian day to an alphanumeric equivalent
	 *          of the form:  MMM DD (JJJ), YYYY
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IYEAR:   Integer year field.
	 *    IJDAY:   Integer day field.
	 *    NCDATE:  Maximum length of output alphanumeric field.
	 *             NCDATE must be at least 18.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KKDATE:  Equivalent alphanumeric date field.
	 *             Set to all asterisks if an error occurred.
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL: GSL/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  GTOUTM, KIDATE
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    KMONTH:  Array containing names of the months [char*4].
	 *    KENC:    Used to convert integers to ASCII [char*4].
	 *=====================================================================
	 * ASSUMPTIONS:
	 * - KIDATE is range checking year/julian day field.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Make sure output character variable is int enough. */

	if( ncdate < 18 ){
		*nerr = 906;
		setmsg( "ERROR", *nerr );
		subscpy( kkdate, 0, ncdate - 1, kkdate_s - 1, "********"
		  );
		goto L_8888;
		}

	/* - Convert from julian day to month and day. */

	kidate( iyear, ijday, &imonth, &iday, nerr );

	/* - Return if input fields were bad. */

	if( *nerr != 0 ){
		setmsg( "ERROR", 911 );
		subscpy( kkdate, 0, ncdate - 1, kkdate_s - 1, "********"
		  );
		goto L_8888;
		}

	/* - Define month subfield: */

	subscpy( kkdate, 0, 3, kkdate_s - 1, kmonth[imonth - 1] );

	/* - Encode day subfield: */

        sprintf(kenc,"%4d",iday);
	if( iday >= 10 ){
                strncpy((s1=malloc(3)),kenc+2,2);
                s1[2] = '\0';
		subscpy( kkdate, 4, 5, kkdate_s - 1, s1 );
                free(s1);
		}
	else{
                cattemp = malloc(3);
                cattemp[0] = '0';
                cattemp[1] = kenc[3];
                cattemp[2] = '\0';
		subscpy( kkdate, 4, 5, kkdate_s - 1, cattemp );
                free(cattemp);
		}

	/* - Encode julian day subfield. */

	subscpy( kkdate, 6, 7, kkdate_s - 1, " (" );
        sprintf(kenc,"%4d",ijday);
	if( ijday >= 100 ){
                strncpy((s1=malloc(4)),kenc+1,3);
                s1[3] = '\0';
		subscpy( kkdate, 8, 10, kkdate_s - 1, s1 );
                free(s1);
		}
	else if( ijday >= 10 ){
		kkdate[8] = '0';
                strncpy((s1=malloc(3)),kenc+2,2);
                s1[2] = '\0';
		subscpy( kkdate, 9, 10, kkdate_s - 1, s1 );
                free(s1);
		}
	else{
		subscpy( kkdate, 8, 9, kkdate_s - 1, "00" );
		kkdate[10] = kenc[3];
		}

	/* - Encode year subfield, blank filling to end of output field. */

	subscpy( kkdate, 11, 13, kkdate_s - 1, "), " );
        sprintf(kenc,"%4d",iyear);
        strncpy((s1=malloc(5)),kenc,4);
        s1[4] = '\0';
	subscpy( kkdate, 14, ncdate - 1, kkdate_s - 1, s1 );
        free(s1);

L_8888:

	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    811019:  Rewrote using F77 character string maninpulations.
	 *    810120:  Changed to output message retrieval from disk.
	 *===================================================================== */

} /* end of function */

