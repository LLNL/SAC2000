#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ kijdat(iyear, imonth, iday, ijday, nerr)
int iyear, imonth, iday, *ijday, *nerr;
{
	int jmonth, jmonth_;
	static int ndays[12]={31,28,31,30,31,30,31,31,30,31,30,31};

	int *const Ndays = &ndays[0] - 1;


	/*=====================================================================
	 * PURPOSE: To convert an integer year, month, and day to its
	 *          julian equivalent.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IYEAR:   Integer year.
	 *    IMONTH:  Integer month.
	 *    IDAY:    Integer day of month.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    IJDAY:   Equivalent integer julian day.
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 0909.
	 *=====================================================================
	 * MODULE/LEVEL: GSL/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NDAYS:   Array of number of days in each month.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Make temporary leap year adjustment to days-in-month table
	 *   if necessary. */

	if( (iyear/4)*4 == iyear ){
		Ndays[2] = 29;
		}
	else{
		Ndays[2] = 28;
		}

	/* - Check month and day fields for validity. */

	if( ((imonth <= 0 || imonth > 12) || iday <= 0) || iday > Ndays[imonth] ){
		*nerr = 909;
		setmsg( "ERROR", *nerr );
		apimsg( iyear );
		apimsg( imonth );
		apimsg( iday );
		*ijday = 0;
		goto L_8888;
		}

	/* - Compute julian day from days-in-month table. */

	*ijday = iday;
	for( jmonth = 1; jmonth <= (imonth - 1); jmonth++ ){
		jmonth_ = jmonth - 1;
		*ijday = *ijday + Ndays[jmonth];
		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    811019:  Cleaned up algorithm and documented module.
	 *    810120:  Changed to output message retrieval from disk.
	 *===================================================================== */

} /* end of function */

