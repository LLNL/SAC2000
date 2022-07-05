#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

/*  Copyright 1991  Regents of the University of California                      
 *
 *
 *  Author:  Dave Harris                                                         
 *  Last Modified:  June 30, 1991                                                
 *
 *           Lawrence Livermore National Laboratory                              
 *           L-205                                                               
 *           P.O. Box 808                                                        
 *           Livermore, CA  94550                                                
 *           USA                                                                 
 *
 *           (415) 423-0617                                                      
 *                                                           iirfilter           
 *  Subroutine to apply an iir filter to a data sequence.                        
 *    The filter is assumed to be stored as second order sections.               
 *    Filtering is in-place.                                                     
 *
 *  input arguments:                                                     
 *  ----------------                                                     
 *
 *    data                       array containing data                   
 *
 *    nsamples                   number of data samples                  
 *
 *    a                          array of gains for second order         
 *                                 sections.                             
 *                                 size:  nsects                         
 *
 *    sn                         numerator polynomials for second        
 *                                 order sections.                       
 *                                 size:  2*nsects                       
 *                                 this subroutine assumes that the      
 *                                 zeroth numerator coefficient is 1     
 *
 *    sd                         denominator polynomials for second      
 *                                 order sections.                       
 *                                 size:  2*nsects                       
 *                                 this subroutine assumes that the      
 *                                 zeroth denominator coefficient is 1   
 *
 *                A second order section looks like:                     
 *
 *                                    -1       -2                        
 *                            1 + b1*z   + b2*z            b1 = sn(2*j-1)
 *                                                         b2 = sn(2*j)  
 *               Sj(z) = g * ---------------------          g = a(j)     
 *                                    -1       -2          a1 = sd(2*j-1)
 *                            1 + a1*z   + a2*z            a2 = sd(2*j)  
 *
 *
 *    nsects                         number of second-order sections     
 *
 *    states                         internal states that must be        
 *                                   saved for blocked data.             
 *                                   must be of length 2*nsects          
 *
 *  output arguments:                                                    
 *  -----------------                                                    
 *
 *    fdata                          filtered data array                 
 *
 * */
void /*FUNCTION*/ iirfilter(data, nsamples, a, sn, sd, nsects, states, 
	 fdata)
float data[];
int nsamples;
float a[], sn[], sd[];
int nsects;
float states[], fdata[];
{
	int i, i_, j, j_, jptr;
	float a1, a2, b1, b2, g, s0, s1, s2;

	float *const A = &a[0] - 1;
	float *const Data = &data[0] - 1;
	float *const Fdata = &fdata[0] - 1;
	float *const Sd = &sd[0] - 1;
	float *const Sn = &sn[0] - 1;
	float *const States = &states[0] - 1;




	jptr = 1;
	for( i = 1; i <= nsamples; i++ ){
		i_ = i - 1;
		Fdata[i] = Data[i];
		}
	for( j = 1; j <= nsects; j++ ){
		j_ = j - 1;

		s1 = States[jptr];
		s2 = States[jptr + 1];
		g = A[j];
		b1 = Sn[jptr];
		b2 = Sn[jptr + 1];
		a1 = Sd[jptr];
		a2 = Sd[jptr + 1];

		for( i = 1; i <= nsamples; i++ ){
			i_ = i - 1;

			s0 = g*Fdata[i] - a1*s1 - a2*s2;
			Fdata[i] = s0 + b1*s1 + b2*s2;
			s2 = s1;
			s1 = s0;

			}

		States[jptr] = s1;
		States[jptr + 1] = s2;

		jptr = jptr + 2;

		}


	return;
} /* end of function */

