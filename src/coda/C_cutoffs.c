#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "coda.h"

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
 *                                                    CUTOFFS                    
 *
 *  Subroutine to alter the cutoff of a filter.  Assumes that the                
 *    filter is structured as second order sections.  Changes                    
 *    the cutoffs of normalized lowpass and highpass filters through             
 *    a simple polynomial transformation.                                        
 *
 *  Input Arguments:                                                             
 *  ----------------                                                             
 *
 *    F                       New cutoff frequency                               
 *
 *  Input/Output Arguments:                                                      
 *  -----------------------                                                      
 *
 *    SN                      Numerator polynomials for second order             
 *                              sections.                                        
 *
 *    SD                      Denominator polynomials for second order           
 *                              sections.                                        
 *
 *    NSECTS                  Number of second order sectionsects                
 *
 * */
void C_cutoffs(float* sn, float* sd, int nsects, double f)
{
	int i, i_, iptr;
	float scale;

	float *const Sd = &sd[0] - 1;
	float *const Sn = &sn[0] - 1;




	scale = 2.*3.14159265*f;

	iptr = 1;
	for( i = 1; i <= nsects; i++ ){
		i_ = i - 1;

		Sn[iptr + 1] = Sn[iptr + 1]/scale;
		Sn[iptr + 2] = Sn[iptr + 2]/(scale*scale);
		Sd[iptr + 1] = Sd[iptr + 1]/scale;
		Sd[iptr + 2] = Sd[iptr + 2]/(scale*scale);
		iptr = iptr + 3;

		}

	return;
} /* end of function */

