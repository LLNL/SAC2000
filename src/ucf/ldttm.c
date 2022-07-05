#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
int /*FUNCTION*/ ldttm(ndttm)
int ndttm[];
{
	int ldttm_v;

	int *const Ndttm = &ndttm[0] - 1;


	/**********************************************************************
	 * PURPOSE: To determine whether a date-time array is valid or not.
	 **********************************************************************
	 * INPUT ARGUMENTS:
	 *    NDTTM:   Date-time array to validate.
	 *           NDTTM(1) = year.
	 *           NDTTM(2) = Julian day.
	 *           NDTTM(3) = hour.
	 *           NDTTM(4) = minute.
	 *           NDTTM(5) = second.
	 *           NDTTM(6) = millisecond.
	 **********************************************************************
	 * FUNCTION VALUE:
	 *    .TRUE.   if date-time array is valid.
	 *    .FALSE.  if not.
	 **********************************************************************
	 * GLOBAL INPUT:
	 *    CLHDR:   NUNDEF
	 **********************************************************************
	 * LIMITATIONS:
	 * - Each element is check to see if it is defined, but is not checked
	 *   to see if it lies within the valid range for that element.
	 *   This feature will be added if it is needed.
	 **********************************************************************
	 * KNOWN ERRORS:
	 ********************************************************************** */
	/* PROCEDURE: */
	/* - Check to make sure each element of the date-time array is defined. */
	if( Ndttm[1] == cmhdr.nundef ){
		ldttm_v = FALSE;
		}
	else if( Ndttm[2] == cmhdr.nundef ){
		ldttm_v = FALSE;
		}
	else if( Ndttm[3] == cmhdr.nundef ){
		ldttm_v = FALSE;
		}
	else if( Ndttm[4] == cmhdr.nundef ){
		ldttm_v = FALSE;
		}
	else if( Ndttm[5] == cmhdr.nundef ){
		ldttm_v = FALSE;
		}
	else if( Ndttm[6] == cmhdr.nundef ){
		ldttm_v = FALSE;
		}
	else{
		ldttm_v = TRUE;
		}

L_8888:
	return( ldttm_v );

	/**********************************************************************
	 * MODIFICATION HISTORY:
	 *    810202:  Original version.
	 ********************************************************************** */

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    810000:  Original version.
	 *===================================================================== */

} /* end of function */

