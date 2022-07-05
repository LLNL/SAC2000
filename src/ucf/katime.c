#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ katime(ihour, imin, isec, imsec, nctime, kktime, 
	 kktime_s, nerr)
int ihour, imin, isec, imsec, nctime;
char *kktime;   int kktime_s;
int *nerr;
{
	char kenc[5];
        char *s1;

	/*=====================================================================
	 * PURPOSE: To convert four integer fields representing hour, minute,
	 *          second, and millisecond to an alphanumeric equivalent
	 *          of the form:  HH:MM:SS.SSS
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IHOUR:   Integer hour field.
	 *    IMIN:    Integer minute field.
	 *    ISEC:    Integer second field.
	 *    IMSEC:   Integer millisecond field.
	 *    NCTIME:  Maximum length of output alphanumeric field.
	 *             NCTIME must be at least 12.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KKTIME:  Equivalent alphanumeric field.
	 *             Set to all asterisks if an error occurs.
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 0905, 0907.
	 *=====================================================================
	 * MODULE/LEVEL: GSL/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  GTOUTM
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    KENC:    Used to convert integers to ascii [char*4].
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Make sure output character variable is int enough. */

	if( nctime < 12 ){
		*nerr = 905;
		setmsg( "ERROR", *nerr );
		subscpy( kktime, 0, nctime - 1, kktime_s - 1, "********"
		  );
		goto L_8888;
		}

	/* - If input integer fields have valid values: */

	if( ((((((ihour >= 0 && ihour <= 24) && imin >= 0) && imin <= 
	 59) && isec >= 0) && isec <= 59) && imsec >= 0) && imsec <= 999 ){

		/* -- Define general form. */
		subscpy( kktime, 0, nctime - 1, kktime_s - 1, "00:00:00.000"
		  );

		/* -- Encode hour field. */
                sprintf(kenc,"%4d",ihour);
		if( ihour >= 10 ){
                        strncpy((s1=malloc(3)),kenc+2,2);
                        s1[2] = '\0';
			subscpy( kktime, 0, 1, kktime_s - 1, s1 );
                        free(s1);
			}
		else{
			kktime[1] = kenc[3];
			}

		/* -- Encode minute field. */
                sprintf(kenc,"%4d",imin);
		if( imin >= 10 ){
                        strncpy((s1=malloc(3)),kenc+2,2);
                        s1[2] = '\0';
			subscpy( kktime, 3, 4, kktime_s - 1, s1 );
                        free(s1);
			}
		else{
			kktime[4] = kenc[3];
			}

		/* -- Encode second field */
                sprintf(kenc,"%4d",isec);
		if( isec >= 10 ){
                        strncpy((s1=malloc(3)),kenc+2,2);
                        s1[2] = '\0';
			subscpy( kktime, 6, 7, kktime_s - 1, s1 );
                        free(s1);
			}
		else{
			kktime[7] = kenc[3];
			}

		/* -- Encode millisecond field. */
                sprintf(kenc,"%4d",imsec);
		if( imsec >= 100 ){
                        strncpy((s1=malloc(4)),kenc+1,3);
                        s1[3] = '\0';
			subscpy( kktime, 9, 11, kktime_s - 1, s1 );
                        free(s1);
			}
		else if( imsec >= 10 ){
                        strncpy((s1=malloc(3)),kenc+2,2);
                        s1[2] = '\0';
			subscpy( kktime, 10, 11, kktime_s - 1, s1 );
                        free(s1);
			}
		else{
			kktime[11] = kenc[3];
			}

		/* - Set up error message if input fields were bad. */

		}
	else{
		*nerr = 907;
		setmsg( "ERROR", *nerr );
		apimsg( ihour );
		apimsg( imin );
		apimsg( isec );
		apimsg( imsec );
		subscpy( kktime, 0, nctime - 1, kktime_s - 1, "********"
		  );
		goto L_8888;
		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    811019:  Made minor changes and added documentation.
	 *    810313:  Rewrite to include leading zeros in all time fields.
	 *    810120:  Changed to output message retrieval from disk.
	 *    801113:  Changed to use of F77 concantenation rather than ZMOVEC.
	 *    801018:  Changed from floating sec. to integer sec. and millisec.
	 *===================================================================== */

} /* end of function */

