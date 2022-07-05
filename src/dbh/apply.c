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
 *                                                               APPLY           
 *  Subroutine to apply an iir filter to a data sequence.                        
 *    The filter is assumed to be stored as second order sections.               
 *    Filtering is in-place.                                                     
 *    Zero-phase (forward and reverse) is an option.                             
 *
 *  Input Arguments:                                                             
 *  ----------------                                                             
 *
 *    DATA                           Array containing data                       
 *
 *    NSAMPS                         Number of data samples                      
 *
 *    ZP                             Logical variable, true for                  
 *                                     zero phase filtering, false               
 *                                     for single pass filtering                 
 *
 *    SN                             Numerator polynomials for second            
 *                                     order sections.                           
 *
 *    SD                             Denominator polynomials for second          
 *                                     order sections.                           
 *
 *    NSECTS                         Number of second-order sections             
 *
 *  Output Arguments:                                                            
 *  -----------------                                                            
 *
 *    DATA                          Data array (same as input)                   
 *
 * */
void /*FUNCTION*/ apply(data, nsamps, zp, sn, sd, nsects)
float data[];
int nsamps;
int zp;
float sn[], sd[];
int nsects;
{
	int i, i_, j, j_, jptr;
	float a1, a2, b0, b1, b2, output, x1, x2, y1, y2;

	float *const Data = &data[0] - 1;
	float *const Sd = &sd[0] - 1;
	float *const Sn = &sn[0] - 1;




	jptr = 1;
	for( j = 1; j <= nsects; j++ ){
		j_ = j - 1;

		x1 = 0.0;
		x2 = 0.0;
		y1 = 0.0;
		y2 = 0.0;
		b0 = Sn[jptr];
		b1 = Sn[jptr + 1];
		b2 = Sn[jptr + 2];
		a1 = Sd[jptr + 1];
		a2 = Sd[jptr + 2];
		for( i = 1; i <= nsamps; i++ ){
			i_ = i - 1;

			output = b0*Data[i] + b1*x1 + b2*x2;
			output = output - (a1*y1 + a2*y2);
			y2 = y1;
			y1 = output;
			x2 = x1;
			x1 = Data[i];
			Data[i] = output;

			}

		jptr = jptr + 3;

		}

	if( zp ){
		jptr = 1;
		for( j = 1; j <= nsects; j++ ){
			j_ = j - 1;

			x1 = 0.0;
			x2 = 0.0;
			y1 = 0.0;
			y2 = 0.0;
			b0 = Sn[jptr];
			b1 = Sn[jptr + 1];
			b2 = Sn[jptr + 2];
			a1 = Sd[jptr + 1];
			a2 = Sd[jptr + 2];

			for( i = nsamps; i >= 1; i-- ){
				i_ = i - 1;

				output = b0*Data[i] + b1*x1 + b2*x2;
				output = output - (a1*y1 + a2*y2);
				y2 = y1;
				y1 = output;
				x2 = x1;
				x1 = Data[i];
				Data[i] = output;

				}

			jptr = jptr + 3;

			}

		}

	return;
} /* end of function */

