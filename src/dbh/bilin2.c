#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

/*  Copyright 1990  Regents of the University of California                      
 *
 *
 *  Author:  Dave Harris                                                         
 *
 *           Lawrence Livermore National Laboratory                              
 *           L-205                                                               
 *           P.O. Box 808                                                        
 *           Livermore, CA  94550                                                
 *           USA                                                                 
 *
 *           (415) 423-0617                                                      
 *
 *
 *  Transforms an analog filter to a digital filter via the bilinear transformati
 *    Assumes both are stored as second order sections.  The transformation is   
 *    done in-place.                                                             
 *
 *  Input Arguments:                                                             
 *  ----------------                                                             
 *
 *    SN                   Array containing numerator polynomial coefficients for
 *                           second order sections.  Packed head-to-tail.        
 *
 *    SD                   Array containing denominator polynomial coefficients f
 *                           second order sections.  Packed head-to-tail.        
 *
 *    NSECTS               Number of second order sections.                      
 *
 * */
void /*FUNCTION*/ bilin2(sn, sd, nsects)
float sn[], sd[];
int nsects;
{
	int i, i_, iptr;
	float a0, a1, a2, scale;

	float *const Sd = &sd[0] - 1;
	float *const Sn = &sn[0] - 1;




	iptr = 1;
	for( i = 1; i <= nsects; i++ ){
		i_ = i - 1;

		a0 = Sd[iptr];
		a1 = Sd[iptr + 1];
		a2 = Sd[iptr + 2];

		scale = a2 + a1 + a0;
		Sd[iptr] = 1.;
		Sd[iptr + 1] = (2.*(a0 - a2))/scale;
		Sd[iptr + 2] = (a2 - a1 + a0)/scale;

		a0 = Sn[iptr];
		a1 = Sn[iptr + 1];
		a2 = Sn[iptr + 2];

		Sn[iptr] = (a2 + a1 + a0)/scale;
		Sn[iptr + 1] = (2.*(a0 - a2))/scale;
		Sn[iptr + 2] = (a2 - a1 + a0)/scale;

		iptr = iptr + 3;

		}

	return;
} /* end of function */

