#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
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
 *                                                                  DESIGN       
 *
 *  Subroutine to design IIR digital filters from analog prototypes.             
 *
 *  Input Arguments:                                                             
 *  ----------------                                                             
 *
 *    IORD                Filter order (10 MAXIMUM)                              
 *
 *    TYPE                Character*2 variable containing filter type            
 *                          LOWPASS (LP)                                         
 *                          HIGHPASS (HP)                                        
 *                          BANDPASS (BP)                                        
 *                          BANDREJECT (BR)                                      
 *
 *    APROTO              Character*2 variable designating analog prototype      
 *                          Butterworth (BU)                                     
 *                          Bessel (BE)                                          
 *                          Chebyshev Type I (C1)                                
 *                          Chebyshev Type II (C2)                               
 *
 *    A                   Chebyshev stopband attenuation factor                  
 *
 *    TRBNDW              Chebyshev transition bandwidth (fraction of            
 *                          lowpass prototype passband width)                    
 *
 *    FL                  Low-frequency cutoff                                   
 *
 *    FH                  High-frequency cutoff                                  
 *
 *    TS                  Sampling interval (in seconds)                         
 *
 *  Output Arguments:                                                            
 *  -----------------                                                            
 *
 *    SN                  Array containing numerator coefficients of             
 *                        second-order sections packed head-to-tail.             
 *
 *    SD                  Array containing denominator coefficients              
 *                        of second-order sections packed head-to-tail.          
 *
 *    NSECTS              Number of second-order sections.                       
 *
 * */
void /*FUNCTION*/ design(iord, type, aproto, a, trbndw, fl, fh, ts, 
	 sn, sd, nsects)
int iord;
char *type, *aproto;
double a, trbndw, fl, fh, ts;
float sn[], sd[];
int *nsects;
{
	char stype[10][4];
	float dcvalue, eps, fhw, flw, omegar, ripple;
	complexf p[10], z[10];

	complexf *const P = &p[0] - 1;
	float *const Sd = &sd[0] - 1;
	float *const Sn = &sn[0] - 1;
	complexf *const Z = &z[0] - 1;




	/*  Analog prototype selection                                                   
	 * */
	if( memcmp(aproto,"BU",2) == 0 ){

		buroots( p, (char*)stype,4, &dcvalue, nsects, iord );

		}
	else if( memcmp(aproto,"BE",2) == 0 ){

		beroots( p, (char*)stype,4, &dcvalue, nsects, iord );

		}
	else if( memcmp(aproto,"C1",2) == 0 ){

		chebparm( a, trbndw, iord, &eps, &ripple );
		c1roots( p, (char*)stype,4, &dcvalue, nsects, iord, eps );

		}
	else if( memcmp(aproto,"C2",2) == 0 ){

		omegar = 1. + trbndw;
		c2roots( p, z, (char*)stype,4, &dcvalue, nsects, iord, a, 
		 omegar );

		}

	/*  Analog mapping selection                                                     
	 * */
	if( memcmp(type,"BP",2) == 0 ){

		flw = warp( fl*ts/2., 2. );
		fhw = warp( fh*ts/2., 2. );
		lptbp( p, z, (char*)stype,4, dcvalue, nsects, flw, fhw, sn, 
		 sd );

		}
	else if( memcmp(type,"BR",2) == 0 ){

		flw = warp( fl*ts/2., 2. );
		fhw = warp( fh*ts/2., 2. );
		lptbr( p, z, (char*)stype,4, dcvalue, nsects, flw, fhw, sn, 
		 sd );

		}
	else if( memcmp(type,"LP",2) == 0 ){

		fhw = warp( fh*ts/2., 2. );
		lp( p, z, (char*)stype,4, dcvalue, *nsects, sn, sd );
		cutoffs( sn, sd, *nsects, fhw );

		}
	else if( memcmp(type,"HP",2) == 0 ){

		flw = warp( fl*ts/2., 2. );
		lpthp( p, z, (char*)stype,4, dcvalue, *nsects, sn, sd );
		cutoffs( sn, sd, *nsects, flw );

		}

	/*  Bilinear analog to digital transformation                                    
	 * */
	bilin2( sn, sd, *nsects );

	return;
} /* end of function */







