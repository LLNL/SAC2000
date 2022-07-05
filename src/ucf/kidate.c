#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ kidate(iyear, ijday, imonth, iday, nerr)
int iyear, ijday, *imonth, *iday, *nerr;
{
	int imonth_;
	static int ndays[12]={31,28,31,30,31,30,31,31,30,31,30,31};

	int *const Ndays = &ndays[0] - 1;


	/*=====================================================================
	 * PURPOSE: To convert an integer year and julian day into an
	 *          equivalent year, month and day.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IYEAR:   Integer year.
	 *    IJDAY:   Integer julian day.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    IMONTH:  Integer month.
	 *    IDAY:    Integer day of month.
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 0909.
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
	 *    NDAYS:   Array containing number of days in each month.
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

	/* - Subtract days-in-month from julian day until proper month is found. */

	*iday = ijday;
	for( *imonth = 1; *imonth <= 12; (*imonth)++ ){
		imonth_ = *imonth - 1;
		if( *iday <= Ndays[*imonth] )
			goto L_8888;
		*iday = *iday - Ndays[*imonth];
		}

	/* - Process error due to bad input here. */

	*nerr = 909;
	setmsg( "ERROR", *nerr );
	apimsg( iyear );
	apimsg( ijday );
	*imonth = 0;
	*iday = 0;

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    811019:  Documention completed.
	 *    810120:  Changed to output message retrieval from disk.
	 *===================================================================== */

} /* end of function */

