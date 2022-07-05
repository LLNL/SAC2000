#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*                                                    RMS
 *
 *  REAL*4 function to compute the rms value of an array of samples
 *
 *  Author:  Dave Harris
 *
 *  Created:  April 16, 1984
 *
 *  Last Modified:  April 16, 1984
 *
 *  Replacements
 *
 *
 *
 *  Input arguments:
 *  ----- ----------                                                            
 *
 *    x                             real*4 array of samples                      
 *
 *    nsamps                        number of samples                            
 *
 *
 *  output arguments:                                                            
 *  ------ ----------                                                            
 *
 *    rms                           rms value of samples                         
 *
 *  linkage:  none                                                               
 *
 * */
double /*FUNCTION*/ rms(x, nsamps)
float x[];
int nsamps;
{
	int i, i_;
	float rms_v;

	float *const X = &x[0] - 1;


	rms_v = 0.;
	for( i = 1; i <= nsamps; i++ ){
		i_ = i - 1;
		rms_v = rms_v + powi(X[i],2);
		}

	rms_v = sqrt( rms_v );

	return( rms_v );

} /* end of function */

