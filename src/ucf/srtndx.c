#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MAX_	512

#include "../../inc/mach.h"
void /*FUNCTION*/ srtndx(value, num, index, nerr)
float value[];
int num, index[], *nerr;
{
	int lagain;
	int i, j, j_, nax;
	float svalue[MAX_], v;

	int *const Index = &index[0] - 1;
	float *const Svalue = &svalue[0] - 1;
	float *const Value = &value[0] - 1;


	/*=====================================================================
	 * PURPOSE:
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:
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
	/* PURPOSE...SORT OF A FLOATING POINT ARRAY. */
	/* PROCEDURE: */
	*nerr = 0;

	/* RANGE CHECK ON NUM */

	if( num <= 0 || num > MAX_ ){
		nax = MAX_;
		*nerr = 910;
		setmsg( "ERROR", *nerr );
		apimsg( nax );
		goto L_8888;
		}

	/* SET UP INDEX AND SCRATCH ARRAYS */

	for( j = 1; j <= num; j++ ){
		j_ = j - 1;
		Svalue[j] = Value[j];
		Index[j] = j;
		}

	/* SORT BOTH SCRATCH AND INDEX ARRAYS BASED ON SCRATCH ARRAY */

L_2000:
	lagain = FALSE;
	for( j = 1; j <= (num - 1); j++ ){
		j_ = j - 1;

                if ((Svalue[j] - Svalue[j + 1]) <= 0.0 )
                                goto L_4000;

L_3000:
		v = Svalue[j];
		Svalue[j] = Svalue[j + 1];
		Svalue[j + 1] = v;
		i = Index[j];
		Index[j] = Index[j + 1];
		Index[j + 1] = i;
		lagain = TRUE;
L_4000:
		;
		}
	if( lagain )
		goto L_2000;

L_8888:
	return;

	/**********************************************************************
	 * MODIFICATION HISTORY:
	 *    800903:  Original version.
	 *    810120:  Changed to output message retrieval from disk.
	 *    810416:  Replaced CMWORK with local storage.
	 ********************************************************************** */

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    810000:  Original version.
	 *===================================================================== */

} /* end of function */

