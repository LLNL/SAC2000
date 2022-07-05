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
 *                                                                         AFR   
 *
 *  Calculates the frequency response of an analog filter:  amplitude, phase     
 *    and group delay. Assumes filter is stored as second order sections.        
 *
 *  Input Arguments:                                                             
 *  ----------------                                                             
 *
 *    SN               Array containing numerator polynomial coefficients for    
 *                       second order sections.  Packed head-to-tail.            
 *
 *    SD               Array containing denominator polynomial coefficients for  
 *                       second order sections.  Packed head-to-tail.            
 *
 *    NSECTS           Number of second order sections.                          
 *
 *    TYPE             Type of response desired (CHARACTER*2):                   
 *                       'AM' amplitude                                          
 *                       'PH' phase                                              
 *                       'GD' group delay                                        
 *
 *    SAMPLING         Frequency domain sampling (CHARACTER*8):                  
 *                       'LINEAR'                                                
 *                       'LOG'                                                   
 *
 *    FL               Low-frequency in range (may not be zero for LOG sampling) 
 *
 *    FH               High-frequency in range (FH > FL)                         
 *
 *    NSAMPS           Number of frequency samples                               
 *
 *
 *  Output Arguments:                                                            
 *  -----------------                                                            
 *
 *    RESPONSE         Array containing desired frequency response samples       
 *
 *    FREQS            Array of frequency sampling points                        
 *
 * */
void /*FUNCTION*/ afr(sn, sd, nsects, type, sampling, fl, fh, nsamps, 
	 response, freqs)
float sn[], sd[];
int nsects;
char *type, *sampling;
double fl, fh;
int nsamps;
float response[], freqs[];
{
	int logspacing;
	int i, i_, iptr, j, j_;
	float a0, a1, a2, afh = 0.0, afl = 0.0, delf, ft, gd, phase, twopi;
	complexf denominator, dent, h, numerator, numt, pd, pdt, s;

	float *const Freqs = &freqs[0] - 1;
	float *const Response = &response[0] - 1;
	float *const Sd = &sd[0] - 1;
	float *const Sn = &sn[0] - 1;





	/*  Frequency sampling initialization                                            
	 * */
	twopi = 2.*3.14159265;
	if( memcmp(sampling,"LOG",3) == 0 ){
		logspacing = TRUE;
		afl = log( fl );
		afh = log( fh );
		}
	else if( memcmp(sampling,"LIN",3) == 0 ){
		logspacing = FALSE;
		afl = fl;
		afh = fh;
		}
	delf = (afh - afl)/(float)( nsamps - 1 );

	ft = afl;

	/*  Loop over frequency                                                          
	 * */
	for( j = 1; j <= nsamps; j++ ){
		j_ = j - 1;

		if( logspacing ){
			Freqs[j] = exp( ft );
			}
		else{
			Freqs[j] = ft;
			}
		s = flttocmplx( 0., twopi*Freqs[j] );
		h = flttocmplx( 1., 0. );
		gd = 0.0;

		iptr = 1;
		for( i = 1; i <= nsects; i++ ){
			i_ = i - 1;

			a0 = Sn[iptr];
			a1 = Sn[iptr + 1];
			a2 = Sn[iptr + 2];
			numerator = cmplxadd(cmplxmul((cmplxadd(cmplxmul(flttocmplx(a2,0.),s),flttocmplx(a1,0.))),
			 s),flttocmplx(a0,0.));
			pd = cmplxadd(cmplxmul(flttocmplx(2*a2,0.),s),flttocmplx(a1,0.));
			if( fabs( cmplxtof( cmplxmul(numerator,cmplxcj( numerator )) ) ) == 
			 0.0 ){
				numt = pd;
				pdt = flttocmplx(2*a2,0.);
				gd = gd - cmplxtof( cmplxdiv(pdt,numt) );
				}
			else{
				gd = gd - cmplxtof( cmplxdiv(pd,numerator) );
				}

			a0 = Sd[iptr];
			a1 = Sd[iptr + 1];
			a2 = Sd[iptr + 2];
			denominator = cmplxadd(cmplxmul((cmplxadd(cmplxmul(flttocmplx(a2,0.),s),
			 flttocmplx(a1,0.))),s),flttocmplx(a0,0.));
			pd = cmplxadd(cmplxmul(flttocmplx(2*a2,0.),s),flttocmplx(a1,0.));
			if( fabs( cmplxtof( cmplxmul(denominator,cmplxcj( denominator )) ) ) == 
			 0.0 ){
				dent = pd;
				pdt = flttocmplx(2*a2,0.);
				gd = gd + cmplxtof( cmplxdiv(pdt,dent) );
				}
			else{
				gd = gd + cmplxtof( cmplxdiv(pd,denominator) );
				}

			h = cmplxdiv(cmplxmul(h,numerator),denominator);

			iptr = iptr + 3;

			}

		if( memcmp(type,"AM",2) == 0 ){
			Response[j] = sqrt( cmplxtof( cmplxmul(cmplxcj( h ),h) ) );
			}
		else if( memcmp(type,"PH",2) == 0 ){
			phase = atan2( aimag( h ), cmplxtof( h ) );
			Response[j] = phase*360./twopi;
			}
		else if( memcmp(type,"GD",2) == 0 ){
			Response[j] = gd;
			}

		ft = ft + delf;

		}

	return;
} /* end of function */

