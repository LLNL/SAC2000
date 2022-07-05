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
 * BUROOTS -- SUBROUTINE TO COMPUTE BUTTERWORTH POLES FOR                        
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
 *      DCVALUE        MAGNITUDE OF FILTER AT ZERO FREQUENCY                     
 *
 *      NSECTS         NUMBER OF SECOND ORDER SECTIONS                           
 *
 *  INPUT ARGUMENTS:                                                             
 *  ----------------                                                             
 *
 *      IORD           DESIRED FILTER ORDER                                      
 *
 * */
void /*FUNCTION*/ buroots(p, rtype, rtype_s, dcvalue, nsects, iord)
complexf p[];
char *rtype;   int rtype_s;
float *dcvalue;
int *nsects, iord;
{
#define RTYPE(I_,J_)	(rtype+(I_)*(rtype_s)+(J_))
	int half, k, k_;
	float angle, pi;

	complexf *const P = &p[0] - 1;




	pi = 3.14159265;

	half = iord/2;

	/* TEST FOR ODD ORDER, AND ADD POLE AT -1                                        
	 * */
	*nsects = 0;
	if( 2*half < iord ){
		P[1] = flttocmplx( -1., 0. );
		fstrncpy( RTYPE(0,0) , rtype_s - 1 , "SP", 2 );
		*nsects = 1;
		}

	for( k = 1; k <= half; k++ ){
		k_ = k - 1;
		angle = pi*(.5 + (float)( 2*k - 1 )/(float)( 2*iord ));
		*nsects = *nsects + 1;
		P[*nsects] = flttocmplx( cos( angle ), sin( angle ) );
		fstrncpy( RTYPE(*nsects - 1,0) , rtype_s - 1 , "CP", 2 );
		}

	*dcvalue = 1.0;

	return;
#undef	RTYPE
} /* end of function */






