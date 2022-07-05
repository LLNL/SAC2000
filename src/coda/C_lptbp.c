#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
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
 *                                                    LPTBP                      
 *
 *  Subroutine to convert an prototype lowpass filter to a bandpass filter via   
 *    the analog polynomial transformation.  The lowpass filter is               
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
 *                              (SP) single real pole  or                        
 *                              (CP) complex conjugate pole pair  or             
 *                              (CPZ) complex conjugate pole/zero pairs          
 *
 *    DCVALUE                 Zero frequency value of filter                     
 *
 *    NSECTS                  Number of second-order sections upon input         
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
 *    NSECTS                  Number of second order sections upon output        
 *                              This subroutine doubles the number of            
 *                              sections.                                        
 *
 * */
void C_lptbp(complexf* p, complexf* z, char* rtype, int rtype_s, double dcvalue, int* nsects, double fl,  double fh, float* sn, float* sd)
{
#define RTYPE(I_,J_)	(rtype+(I_)*(rtype_s)+(J_))
	int i, i_, iptr, n;
	float a, b, pi, scale, twopi;
	complexf ctemp, h, p1, p2, s, z1, z2;

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

			ctemp = cmplxsub(cmplxpow((cmplxmul(flttocmplx(b,0.),Z[i])),(double)2),
			 flttocmplx(4.*a,0.));
			ctemp = cmplxsqrt( ctemp );
			z1 = cmplxmul(flttocmplx(0.5,0.),(cmplxadd(cmplxmul(flttocmplx(b,0.),Z[i]),
			 ctemp)));
			z2 = cmplxmul(flttocmplx(0.5,0.),(cmplxsub(cmplxmul(flttocmplx(b,0.),Z[i]),
			 ctemp)));
			ctemp = cmplxsub(cmplxpow((cmplxmul(flttocmplx(b,0.),P[i])),(double)2),
			 flttocmplx(4.*a,0.));
			ctemp = cmplxsqrt( ctemp );
			p1 = cmplxmul(flttocmplx(0.5,0.),(cmplxadd(cmplxmul(flttocmplx(b,0.),P[i]),
			 ctemp)));
			p2 = cmplxmul(flttocmplx(0.5,0.),(cmplxsub(cmplxmul(flttocmplx(b,0.),P[i]),
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

			ctemp = cmplxsub(cmplxpow((cmplxmul(flttocmplx(b,0.),P[i])),(double)2),
			 flttocmplx(4.*a,0.));
			ctemp = cmplxsqrt( ctemp );
			p1 = cmplxmul(flttocmplx(0.5,0.),(cmplxadd(cmplxmul(flttocmplx(b,0.),P[i]),
			 ctemp)));
			p2 = cmplxmul(flttocmplx(0.5,0.),(cmplxsub(cmplxmul(flttocmplx(b,0.),P[i]),
			 ctemp)));
			Sn[iptr] = 0.;
			Sn[iptr + 1] = b;
			Sn[iptr + 2] = 0.;
			Sd[iptr] = cmplxtof( cmplxmul(p1,cmplxcj( p1 )) );
			Sd[iptr + 1] = -2.*cmplxtof( p1 );
			Sd[iptr + 2] = 1.;
			iptr = iptr + 3;
			Sn[iptr] = 0.;
			Sn[iptr + 1] = b;
			Sn[iptr + 2] = 0.;
			Sd[iptr] = cmplxtof( cmplxmul(p2,cmplxcj( p2 )) );
			Sd[iptr + 1] = -2.*cmplxtof( p2 );
			Sd[iptr + 2] = 1.;
			iptr = iptr + 3;

			*nsects = *nsects + 2;

			}
		else if( memcmp(RTYPE(i_,0),"SP",2) == 0 ){

			Sn[iptr] = 0.;
			Sn[iptr + 1] = b;
			Sn[iptr + 2] = 0.;
			Sd[iptr] = a;
			Sd[iptr + 1] = -b*cmplxtof( P[i] );
			Sd[iptr + 2] = 1.;
			iptr = iptr + 3;

			*nsects = *nsects + 1;

			}

		}

	/*  Scaling - use the fact that the bandpass filter amplitude at sqrt( omega_l * 
	 *            equals the amplitude of the lowpass prototype at d.c.              
	 * */
	s = flttocmplx( 0., sqrt( a ) );
	h = flttocmplx( 1., 0. );

	iptr = 1;
	for( i = 1; i <= *nsects; i++ ){
		i_ = i - 1;
		h = cmplxdiv(cmplxmul(h,(cmplxadd(cmplxmul((cmplxadd(cmplxmul(flttocmplx(Sn[iptr + 2],0.),
		 s),flttocmplx(Sn[iptr + 1],0.))),s),flttocmplx(Sn[iptr],0.)))),(cmplxadd(cmplxmul((cmplxadd(cmplxmul(flttocmplx(Sd[iptr + 2],0.),
		 s),flttocmplx(Sd[iptr + 1],0.))),s),flttocmplx(Sd[iptr],0.))));
		iptr = iptr + 3;
		}
	scale = cmplxtof(cmplxdiv(flttocmplx(dcvalue,0.),cmplxsqrt( cmplxmul(flttocmplx(cmplxtof( h ),0.),
	 cmplxcj( h )) )));

	Sn[1] = Sn[1]*scale;
	Sn[2] = Sn[2]*scale;
	Sn[3] = Sn[3]*scale;

	return;
#undef	RTYPE
} /* end of function */

