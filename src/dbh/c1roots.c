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
 * C1ROOTS -- SUBROUTINE TO COMPUTE CHEBYSHEV TYPE I POLES FOR                   
 *   NORMALIZED LOWPASS FILTER                                                   
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
 *      RTYPE          CHARACTER ARRAY INDICATING 2ND ORDER SECTION              
 *                       TYPE:                                                   
 *                         (SP)  SINGLE REAL POLE                                
 *                         (CP)  COMPLEX CONJUGATE POLE PAIR                     
 *                         (CPZ) COMPLEX CONJUGATE POLE-ZERO PAIRS               
 *
 *      DCVALUE        RESPONSE OF FILTER AT ZERO FREQUENCY                      
 *
 *      NSECTS         NUMBER OF SECOND ORDER SECTIONS                           
 *
 *  INPUT ARGUMENTS:                                                             
 *  ----------------                                                             
 *
 *      IORD           DESIRED FILTER ORDER                                      
 *
 *      EPS            CHEBYSHEV PARAMETER RELATED TO PASSBAND RIPPLE            
 * */
void /*FUNCTION*/ c1roots(p, rtype, rtype_s, dcvalue, nsects, iord, 
	 eps)
complexf p[];
char *rtype;   int rtype_s;
float *dcvalue;
int *nsects, iord;
double eps;
{
#define RTYPE(I_,J_)	(rtype+(I_)*(rtype_s)+(J_))
	int half, i, i_;
	float angle, c, gamma, omega, pi, s, sigma;

	complexf *const P = &p[0] - 1;




	pi = 3.14159265;
	half = iord/2;

	/*  INTERMEDIATE DESIGN PARAMETERS                                               
	 * */
	gamma = (1. + sqrt( 1. + eps*eps ))/eps;
	gamma = log( gamma )/(float)( iord );
	gamma = exp( gamma );
	s = .5*(gamma - 1./gamma);
	c = .5*(gamma + 1./gamma);

	/*  CALCULATE POLES                                                              
	 * */
	*nsects = 0;
	for( i = 1; i <= half; i++ ){
		i_ = i - 1;
		fstrncpy( RTYPE(i_,0) , rtype_s - 1 , "CP", 2 );
		angle = (float)( 2*i - 1 )*pi/(float)( 2*iord );
		sigma = -s*sin( angle );
		omega = c*cos( angle );
		P[i] = flttocmplx( sigma, omega );
		*nsects = *nsects + 1;
		}
	if( 2*half < iord ){
		fstrncpy( RTYPE(half,0) , rtype_s - 1 , "SP", 2 );
		P[half + 1] = flttocmplx( -s, 0.0 );
		*nsects = *nsects + 1;
		*dcvalue = 1.0;
		}
	else{
		*dcvalue = 1./sqrt( 1 + powi(eps,2) );
		}

	/*  DONE                                                                         
	 * */
	return;
#undef	RTYPE
} /* end of function */

