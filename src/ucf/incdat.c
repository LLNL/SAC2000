#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

int isLeapYear( int yr);

void /*FUNCTION*/ incdat(nyrold, njdold, njdinc, nyrnew, njdnew)
int nyrold, njdold, njdinc, *nyrnew, *njdnew;
{
	int ndays;

	/* Ind
	 *=====================================================================
	 * PURPOSE:
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * ASSUMPTIONS:
	 *=====================================================================
	 * LIMITATIONS:
	 *=====================================================================
	 * KNOWN ERRORS:
	 *=====================================================================
	 * EXTERNAL DEPENDENCIES:
	 *===================================================================== */
	/* PURPOSE: TO GET A NEW DATE FROM AN OLD DATE AND AN INCREMENT IN DAYS */
	/*    NYROLD:     OLD YEAR
	 *    NJDOLD:     OLD JULIAN DAY
	 *    NJDINC:     INCREMENT IN DAYS (POSITIVE OR NEGATIVE)
	 *    NYRNEW:=    NEW YEAR
	 *    NJDNEW:=    NEW JULIAN DAY */
	/* PROCEDURE: */
	*nyrnew = nyrold;
	*njdnew = njdold + njdinc;

	while ( 1 ) {
	    if( *njdnew < 1 ){
		(*nyrnew) -= 1 ;
		ndays = isLeapYear( *nyrnew ) ? 366 : 365;
		*njdnew += ndays;
	    }
	    else{
		ndays = isLeapYear( *nyrnew ) ? 366 : 365;
		if( *njdnew > ndays ){
		    *nyrnew = *nyrnew + 1;
		    *njdnew = *njdnew - ndays;
		}
		else{
		    return;
		}
	    }
	}
	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    810000:  Original version.
	 *===================================================================== */

} /* end of function */

