#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*                                                           DEWIT
 *
 *  Dewhitens an input sequence in-place.  Uses a low-order prediction 
 *    filter from prewit.
 *
 *  Author:  George Randall from prewit by Dave Harris
 *
 *  Created:  February 10, 1987
 *
 *  Last Modified:  February 10, 1987
 *
 *  Input arguments:
 *  ----- ----------
 *
 *    DATA                 REAL*4 array containing input sequence - contains
 *                         dewhitened sequence upon exit from this routine.
 *
 *    #SAMPLES             Number of data points in sequence.
 *
 *    PREDICTOR_ORDER      Order of prediction filter used to dewhiten
 *                         sequence.
 *
 *  Output Arguments:
 *  ------ ----------
 *
 *    DATA                 As above.
 *
 *    A                    Array of dewhitening filter coefficients.
 *
 *    ERROR_MESSAGE        CHARACTER*130 variable containing error message if
 *                         error is detected.  Equal to ' ' if no errors.
 *
 *  Linkage:     PREDFL
 *
 * */
void /*FUNCTION*/ dewit( data, nsamps, order, a, errmsg )
float data[];
int nsamps, order;
float a[];
char *errmsg;
{
	char temp[ 51 ] ;
	int i ;
	float atemp[13];

	float *const A = &a[0] - 1;
	float *const Atemp = &atemp[0] - 1;
	float *const Data = &data[0] - 1;




	/*  Initializations
	 * */
	errmsg[ 0 ] = temp[ 0 ] = '\0' ;

	/*  Range check for predictor order
	 * */
	if( order < 1 || order > 12 ){
	    strcpy( errmsg,
             "*** DEWIT  -  Predictor order out of bounds (1-12)  ***" );
	}

	/*  Dewhiten the data
	 *
	 *    Negate coefficients of prediction error filter generated by LEVIN.
	 *    For historical reasons, different storage modes are used in LEVIN
	 *    and PEF and PREDFL.
	 * */
	for( i = 1; i <= order; i++ ){
	    Atemp[i] = -A[i + 1];
	}

	predfl( data, nsamps, atemp, order, data, temp );
	if( temp[ 0 ] ){
	    strcat( errmsg, temp ) ;
	    strcat( errmsg, " from (DEWIT)" );
	}

	return;
} /* end of function */

