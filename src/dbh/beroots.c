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
 * BEROOTS -- SUBROUTINE TO RETURN BESSEL POLES FOR                              
 *   NORMALIZED LOWPASS FILTER                                                   
 *
 * LAST MODIFIED:  April 15, 1992. Changed P and RTYPE to adjustable 
 *                 array by using an "*" rather than a "1".     
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
void /*FUNCTION*/ beroots(p, rtype, rtype_s, dcvalue, nsects, iord)
complexf p[];
char *rtype;   int rtype_s;
float *dcvalue;
int *nsects, iord;
{
#define RTYPE(I_,J_)	(rtype+(I_)*(rtype_s)+(J_))

	complexf *const P = &p[0] - 1;




	if( iord == 1 ){

		P[1] = flttocmplx( -1.0, 0.0 );
		fstrncpy( RTYPE(0,0) , rtype_s - 1 , "SP", 2 );

		}
	else if( iord == 2 ){

		P[1] = flttocmplx( -1.1016013, 0.6360098 );
		fstrncpy( RTYPE(0,0) , rtype_s - 1 , "CP", 2 );

		}
	else if( iord == 3 ){

		P[1] = flttocmplx( -1.0474091, 0.9992645 );
		fstrncpy( RTYPE(0,0) , rtype_s - 1 , "CP", 2 );
		P[2] = flttocmplx( -1.3226758, 0.0 );
		fstrncpy( RTYPE(1,0) , rtype_s - 1 , "SP", 2 );

		}
	else if( iord == 4 ){

		P[1] = flttocmplx( -0.9952088, 1.2571058 );
		fstrncpy( RTYPE(0,0) , rtype_s - 1 , "CP", 2 );
		P[2] = flttocmplx( -1.3700679, 0.4102497 );
		fstrncpy( RTYPE(1,0) , rtype_s - 1 , "CP", 2 );

		}
	else if( iord == 5 ){

		P[1] = flttocmplx( -0.9576766, 1.4711244 );
		fstrncpy( RTYPE(0,0) , rtype_s - 1 , "CP", 2 );
		P[2] = flttocmplx( -1.3808774, 0.7179096 );
		fstrncpy( RTYPE(1,0) , rtype_s - 1 , "CP", 2 );
		P[3] = flttocmplx( -1.5023160, 0.0 );
		fstrncpy( RTYPE(2,0) , rtype_s - 1 , "SP", 2 );

		}
	else if( iord == 6 ){

		P[1] = flttocmplx( -0.9306565, 1.6618633 );
		fstrncpy( RTYPE(0,0) , rtype_s - 1 , "CP", 2 );
		P[2] = flttocmplx( -1.3818581, 0.9714719 );
		fstrncpy( RTYPE(1,0) , rtype_s - 1 , "CP", 2 );
		P[3] = flttocmplx( -1.5714904, 0.3208964 );
		fstrncpy( RTYPE(2,0) , rtype_s - 1 , "CP", 2 );

		}
	else if( iord == 7 ){

		P[1] = flttocmplx( -0.9098678, 1.8364514 );
		fstrncpy( RTYPE(0,0) , rtype_s - 1 , "CP", 2 );
		P[2] = flttocmplx( -1.3789032, 1.1915667 );
		fstrncpy( RTYPE(1,0) , rtype_s - 1 , "CP", 2 );
		P[3] = flttocmplx( -1.6120388, 0.5892445 );
		fstrncpy( RTYPE(2,0) , rtype_s - 1 , "CP", 2 );
		P[4] = flttocmplx( -1.6843682, 0.0 );
		fstrncpy( RTYPE(3,0) , rtype_s - 1 , "SP", 2 );

		}
	else if( iord == 8 ){

		P[1] = flttocmplx( -0.8928710, 1.9983286 );
		fstrncpy( RTYPE(0,0) , rtype_s - 1 , "CP", 2 );
		P[2] = flttocmplx( -1.3738431, 1.3883585 );
		fstrncpy( RTYPE(1,0) , rtype_s - 1 , "CP", 2 );
		P[3] = flttocmplx( -1.6369417, 0.8227968 );
		fstrncpy( RTYPE(2,0) , rtype_s - 1 , "CP", 2 );
		P[4] = flttocmplx( -1.7574108, 0.2728679 );
		fstrncpy( RTYPE(3,0) , rtype_s - 1 , "CP", 2 );

		}

	*nsects = iord - iord/2;

	*dcvalue = 1.0;

	/*  DONE                                                                         
	 * */
	return;
#undef	RTYPE
} /* end of function */

