#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include<string.h>
#include "coda_complex.h"

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
 *                                                    LPTBR                      
 *
 *  Subroutine to convert a lowpass filter to a band reject filter               
 *    via an analog polynomial transformation.  The lowpass filter is            
 *    described in terms of its poles and zeros (as input to this routine).      
 *    The output consists of the parameters for second order sections.           
 *
 *  Input Arguments:                                                             
 *  ----------------                                                             
 *
 *    P                       Array containing poles                             
 *
 *    Z                       Array containing zeros                             
 *
 *    RTYPE                   Character array containing type information        
 *                              (SP)  single real pole or                        
 *                              (CP)  complex conjugate pole pair                
 *                              (CPZ) complex conjugate pole/zero pairs          
 *
 *    DCVALUE                 Zero-frequency value of prototype filter           
 *
 *    NSECTS                  Number of second-order sections                    
 *                              prior to transformation                          
 *
 *    FL                      Low-frequency cutoff                               
 *
 *    FH                      High-frequency cutoff                              
 *
 *  Output Arguments:                                                            
 *  -----------------                                                            
 *
 *    SN                      Numerator polynomials for second order             
 *                              sections.                                        
 *
 *    SD                      Denominator polynomials for second order           
 *                              sections.                                        
 *
 *    NSECTS                  Number of second order sections following          
 *                              transformation.  The number is doubled.          
 *
 * */
void C_lptbr(complexf* p, complexf* z, char* rtype, int rtype_s, double dcvalue, int* nsects, double fl,  double fh, float* sn, float* sd)
{
#define RTYPE(I_,J_)	(rtype+(I_)*(rtype_s)+(J_))
	int i, i_, iptr, n;
	float a, b, h, pi, scale, twopi;
	complexf cinv, ctemp, p1, p2, z1, z2;

	complexf *const P = &p[0] - 1;
	float *const Sd = &sd[0] - 1;
	float *const Sn = &sn[0] - 1;
	complexf *const Z = &z[0] - 1;




	pi = 3.14159265;
	twopi = 2.*pi;
	a = twopi*twopi*fl*fh;
	b = twopi*(fh - fl);

	n = *nsects;
	*nsects = 0;
	iptr = 1;
	for( i = 1; i <= n; i++ ){
		i_ = i - 1;

		if( memcmp(RTYPE(i_,0),"CPZ",3) == 0 ){

			cinv = cmplxdiv(flttocmplx(1.,0.),Z[i]);
			ctemp = cmplxsub(cmplxpow((cmplxmul(flttocmplx(b,0.),cinv)),(double)2),
			 flttocmplx(4.*a,0.));
			ctemp = cmplxsqrt( ctemp );
			z1 = cmplxmul(flttocmplx(0.5,0.),(cmplxadd(cmplxmul(flttocmplx(b,0.),cinv),
			 ctemp)));
			z2 = cmplxmul(flttocmplx(0.5,0.),(cmplxsub(cmplxmul(flttocmplx(b,0.),cinv),
			 ctemp)));
			cinv = cmplxdiv(flttocmplx(1.,0.),P[i]);
			ctemp = cmplxsub(cmplxpow((cmplxmul(flttocmplx(b,0.),cinv)),(double)2),
			 flttocmplx(4.*a,0.));
			ctemp = cmplxsqrt( ctemp );
			p1 = cmplxmul(flttocmplx(0.5,0.),(cmplxadd(cmplxmul(flttocmplx(b,0.),cinv),
			 ctemp)));
			p2 = cmplxmul(flttocmplx(0.5,0.),(cmplxsub(cmplxmul(flttocmplx(b,0.),cinv),
			 ctemp)));
			Sn[iptr] = cmplxtof( cmplxmul(z1,cmplxcj( z1 )) );
			Sn[iptr + 1] = -2.*cmplxtof( z1 );
			Sn[iptr + 2] = 1.;
			Sd[iptr] = cmplxtof( cmplxmul(p1,cmplxcj( p1 )) );
			Sd[iptr + 1] = -2.*cmplxtof( p1 );
			Sd[iptr + 2] = 1.;
			iptr = iptr + 3;
			Sn[iptr] = cmplxtof( cmplxmul(z2,cmplxcj( z2 )) );
			Sn[iptr + 1] = -2.*cmplxtof( z2 );
			Sn[iptr + 2] = 1.;
			Sd[iptr] = cmplxtof( cmplxmul(p2,cmplxcj( p2 )) );
			Sd[iptr + 1] = -2.*cmplxtof( p2 );
			Sd[iptr + 2] = 1.;
			iptr = iptr + 3;

			*nsects = *nsects + 2;

			}
		else if( memcmp(RTYPE(i_,0),"CP",2) == 0 ){

			cinv = cmplxdiv(flttocmplx(1.,0.),P[i]);
			ctemp = cmplxsub(cmplxpow((cmplxmul(flttocmplx(b,0.),cinv)),(double)2),
			 flttocmplx(4.*a,0.));
			ctemp = cmplxsqrt( ctemp );
			p1 = cmplxmul(flttocmplx(0.5,0.),(cmplxadd(cmplxmul(flttocmplx(b,0.),cinv),
			 ctemp)));
			p2 = cmplxmul(flttocmplx(0.5,0.),(cmplxsub(cmplxmul(flttocmplx(b,0.),cinv),
			 ctemp)));
			Sn[iptr] = a;
			Sn[iptr + 1] = 0.;
			Sn[iptr + 2] = 1.;
			Sd[iptr] = cmplxtof( cmplxmul(p1,cmplxcj( p1 )) );
			Sd[iptr + 1] = -2.*cmplxtof( p1 );
			Sd[iptr + 2] = 1.;
			iptr = iptr + 3;
			Sn[iptr] = a;
			Sn[iptr + 1] = 0.;
			Sn[iptr + 2] = 1.;
			Sd[iptr] = cmplxtof( cmplxmul(p2,cmplxcj( p2 )) );
			Sd[iptr + 1] = -2.*cmplxtof( p2 );
			Sd[iptr + 2] = 1.;
			iptr = iptr + 3;

			*nsects = *nsects + 2;

			}
		else if( memcmp(RTYPE(i_,0),"SP",2) == 0 ){

			Sn[iptr] = a;
			Sn[iptr + 1] = 0.;
			Sn[iptr + 2] = 1.;
			Sd[iptr] = -a*cmplxtof( P[i] );
			Sd[iptr + 1] = b;
			Sd[iptr + 2] = -cmplxtof( P[i] );
			iptr = iptr + 3;

			*nsects = *nsects + 1;

			}

		}

	/*  Scaling - use the fact that the bandreject filter amplitude  at d.c.         
	 *            equals the lowpass prototype amplitude at d.c.                     
	 * */
	h = 1.0;

	iptr = 1;
	for( i = 1; i <= *nsects; i++ ){
		i_ = i - 1;
		h = h*Sn[iptr]/Sd[iptr];
		iptr = iptr + 3;
		}
	scale = dcvalue/fabs( h );
	Sn[1] = Sn[1]*scale;
	Sn[2] = Sn[2]*scale;
	Sn[3] = Sn[3]*scale;

	return;
#undef	RTYPE
} /* end of function */

