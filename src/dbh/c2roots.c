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
 * C2ROOTS -- SUBROUTINE TO COMPUTE ROOTS FOR NORMALIZED LOWPASS                 
 *   CHEBYSHEV TYPE 2 FILTER                                                     
 *
 * LAST MODIFIED:  SEPTEMBER 7, 1990                                             
 *
 *  OUTPUT ARGUMENTS:                                                            
 *  -----------------                                                            
 *      P              COMPLEX ARRAY CONTAINING POLES                            
 *                       CONTAINS ONLY ONE FROM EACH                             
 *                       COMPLEX CONJUGATE PAIR, AND                             
 *                       ALL REAL POLES                                          
 *
 *      Z              COMPLEX ARRAY CONTAINING ZEROS                            
 *                       CONTAINS ONLY ONE FROM EACH                             
 *                       COMPLEX CONJUGATE PAIR, AND                             
 *                       ALL REAL ZEROS                                          
 *
 *      RTYPE          CHARACTER ARRAY INDICATING 2ND ORDER SECTION              
 *                       TYPE:                                                   
 *                         (SP)  SINGLE REAL POLE                                
 *                         (CP)  COMPLEX CONJUGATE POLE PAIR                     
 *                         (CPZ) COMPLEX CONJUGATE POLE-ZERO PAIRS               
 *
 *      DCVALUE        MAGNITUDE OF FILTER AT ZERO FREQUENCY                     
 *
 *      NSECTS         NUMBER OF SECOND ORDER SECTIONS                           
 *
 *  INPUT ARGUMENTS:                                                             
 *  ----------------                                                             
 *
 *
 *      IORD           DESIRED FILTER ORDER                                      
 *
 *      A              STOPBAND ATTENUATION FACTOR                               
 *
 *      OMEGAR         CUTOFF FREQUENCY OF STOPBAND                              
 *                     PASSBAND CUTOFF IS AT 1.0 HERTZ                           
 *
 * */
void /*FUNCTION*/ c2roots(p, z, rtype, rtype_s, dcvalue, nsects, iord, 
	 a, omegar)
complexf p[], z[];
char *rtype;   int rtype_s;
float *dcvalue;
int *nsects, iord;
double a, omegar;
{
#define RTYPE(I_,J_)	(rtype+(I_)*(rtype_s)+(J_))
	int half, i, i_;
	float alpha, angle, beta, c, denom, gamma, omega, pi, s, sigma;

	complexf *const P = &p[0] - 1;
	complexf *const Z = &z[0] - 1;




	pi = 3.14159265;
	half = iord/2;

	/*  INTERMEDIATE DESIGN PARAMETERS                                               
	 * */
	gamma = a + sqrt( a*a - 1. );
	gamma = log( gamma )/(float)( iord );
	gamma = exp( gamma );
	s = .5*(gamma - 1./gamma);
	c = .5*(gamma + 1./gamma);

	*nsects = 0;
	for( i = 1; i <= half; i++ ){
		i_ = i - 1;

		/*  CALCULATE POLES                                                              
		 * */
		fstrncpy( RTYPE(i_,0) , rtype_s - 1 , "CPZ", 2 );

		angle = (float)( 2*i - 1 )*pi/(float)( 2*iord );
		alpha = -s*sin( angle );
		beta = c*cos( angle );
		denom = alpha*alpha + beta*beta;
		sigma = omegar*alpha/denom;
		omega = -omegar*beta/denom;
		P[i] = flttocmplx( sigma, omega );

		/*  CALCULATE ZEROS                                                              
		 * */
		omega = omegar/cos( angle );
		Z[i] = flttocmplx( 0.0, omega );

		*nsects = *nsects + 1;

		}

	/*  ODD-ORDER FILTERS                                                            
	 * */
	if( 2*half < iord ){
		fstrncpy( RTYPE(half,0) , rtype_s - 1 , "SP", 2 );
		P[half + 1] = flttocmplx( -omegar/s, 0.0 );
		*nsects = *nsects + 1;
		}

	/*  DC VALUE                                                                     
	 * */
	*dcvalue = 1.0;

	/*  DONE                                                                         
	 * */
	return;
#undef	RTYPE
} /* end of function */

