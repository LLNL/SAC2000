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
 *  CHEBPARM - Calculates Chebyshev type I and II design parameters              
 *
 *
 *  INPUT ARGUMENTS                                                              
 *  ---------------                                                              
 *
 *       A                Desired stopband attenuation                           
 *                          i.e. max stopband amplitude is 1/ATTEN               
 *
 *       TRBNDW           Transition bandwidth between stop and passbands        
 *                          as a fraction of the passband width                  
 *
 *       IORD             Filter order (number of poles)                         
 *
 *
 *  OUTPUT ARGUMENTS                                                             
 *  ----------------                                                             
 *
 *       EPS              Chebyshev passband parameter                           
 *
 *       RIPPLE           Passband ripple                                        
 * */
void /*FUNCTION*/ chebparm(a, trbndw, iord, eps, ripple)
double a, trbndw;
int iord;
float *eps, *ripple;
{
	float alpha, g, omegar;

	omegar = 1. + trbndw;
	alpha = powi(omegar + sqrt( powi(omegar,2) - 1. ),iord);
	g = (powi(alpha,2) + 1.)/(2.*alpha);
	*eps = sqrt( powi(a,2) - 1. )/g;
	*ripple = 1./sqrt( 1. + powi(*eps,2) );

	return;
} /* end of function */

