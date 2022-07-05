#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "coda_complex.h"

void C_buroots(complexf* p, char* rtype, int rtype_s, float* dcvalue, int* nsects, int iord);
void C_lptbp(complexf* p, complexf* z, char* rtype, int rtype_s, double dcvalue, int* nsects, double fl,  double fh, float* sn, float* sd);
void C_lptbr(complexf* p, complexf* z, char* rtype, int rtype_s, double dcvalue, int* nsects, double fl,  double fh, float* sn, float* sd);
void C_lp(complexf* p, complexf* z, char* rtype, int rtype_s, double dcvalue, int nsects, float* sn, float* sd);
void C_cutoffs(float* sn, float* sd, int nsects, double f);
void C_lpthp(complexf* p, complexf* z, char* rtype, int rtype_s, double dcvalue, int nsects, float* sn,  float* sd);
void C_bilin2(float* sn, float* sd, int nsects);

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
void C_design(int iord, char* type, char* aproto, double a, double trbndw, double fl, double fh, double ts,  float* sn, float* sd, int* nsects)
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

		C_buroots( p, (char*)stype,4, &dcvalue, nsects, iord );

		}

	/*  Analog mapping selection                                                     
	 * */
	if( memcmp(type,"BP",2) == 0 ){

		flw = C_warp( fl*ts/2., 2. );
		fhw = C_warp( fh*ts/2., 2. );
		C_lptbp( p, z, (char*)stype,4, dcvalue, nsects, flw, fhw, sn, 
		 sd );

		}
	else if( memcmp(type,"BR",2) == 0 ){

		flw = C_warp( fl*ts/2., 2. );
		fhw = C_warp( fh*ts/2., 2. );
		C_lptbr( p, z, (char*)stype,4, dcvalue, nsects, flw, fhw, sn, 
		 sd );

		}
	else if( memcmp(type,"LP",2) == 0 ){

		fhw = C_warp( fh*ts/2., 2. );
		C_lp( p, z, (char*)stype,4, dcvalue, *nsects, sn, sd );
		C_cutoffs( sn, sd, *nsects, fhw );

		}
	else if( memcmp(type,"HP",2) == 0 ){

		flw = C_warp( fl*ts/2., 2. );
		C_lpthp( p, z, (char*)stype,4, dcvalue, *nsects, sn, sd );
		C_cutoffs( sn, sd, *nsects, flw );

		}

	/*  Bilinear analog to digital transformation                                    
	 * */
	C_bilin2( sn, sd, *nsects );

	return;
} /* end of function */







