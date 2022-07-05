#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "complex.h"
#include "proto.h"

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
 *                                                                         DFR
 *
 *  Calculates the frequency response of a digital filter:  amplitude, phase
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
 *    NSAMPS           Number of frequency samples                      
 *
 *    TS               Sampling interval (seconds)                     
 *
 *  Output Arguments:                                                 
 *  -----------------                                                
 *
 *    RESPONSE         Array containing desired frequency response  
 *
 * */
void /*FUNCTION*/ dfr(sn, sd, nsects, type, nsamps, ts, response)
float sn[], sd[];
int nsects;
char *type;
int nsamps;
double ts;
float response[];
{
	int i, iptr, j ;
	float a0, a1, a2, delomega, gd, omega, phase, pi;
	complexf denominator, h, numerator, pd, z;
        float localGD ;

	float *const Response = &response[0] - 1;
	float *const Sd = &sd[0] - 1;
	float *const Sn = &sn[0] - 1;



	pi = 3.14159265;
	delomega = pi/(float)( nsamps - 1 );
	omega = 0.0 ;

	/*  Loop over frequency                                                          
	 * */
	for( j = 1; j <= nsamps; j++ ){
	    z = flttocmplx( cos( omega ), -sin( omega ) );
	    h = flttocmplx( 1., 0. );
	    gd = 0.0;

	    iptr = 1;
	    for( i = 1; i <= nsects; i++ ){
		complexf temp1, temp2, temp3, temp4, temp5, temp6 ;

		a0 = Sn[iptr];
		a1 = Sn[iptr + 1];
		a2 = Sn[iptr + 2];

		/* numerator = a2(z*z) + a1(z) + a0 */
		temp1 = flttocmplx(a1,0.) ;
		temp2 = flttocmplx(a2,0.) ;
		temp3 = cmplxmul(temp2,z) ;
		temp4 = cmplxadd(temp3,temp1) ;
		temp5 = cmplxmul(temp4, z) ;
		temp6 = flttocmplx(a0,0.) ;
		numerator = cmplxadd(temp5,temp6);

		/* pd = 2*a2(z*z) + a1(z) */
                temp1 = flttocmplx(a2*2.,0.) ;
                temp2 = cmplxmul(temp1,z) ;
                temp3 = flttocmplx(a1,0.) ;
                temp4 = cmplxadd(temp2,temp3) ;
                pd = cmplxmul(temp4, z);


		/* As z -> 1, both pd and numerator -> 0 + 0i, apply
		   l'Hopital's rule, to avoid dividing by zero.
		   Note: the derivatives in l'Hopital's rule are taken
		   with respect to omega (denoted as w) the chain rule
		   provides that dn/dw = (dn/dz)(dz/dw)
		   in this case, dz/dw = -jz
		   let n denote numerator and p denote pd:

		   (dp/dz)(dz/dw) = ( 4*a2(z) + a1 ) * (-jz)
		   (dn/dz)(dz/dw) = ( 2*a2(z) + a1 ) * (-jz)

		   letting a2 = -2a1
		   (as it always does the first time throught the loop)

		                                          4*a2(z) - 2a2
		   [ (dp/dz)(dz/dw) / (dn/dz)(dz/dw) ] = ---------------
		                                          2*a2(z) - 2a2

		                                          2z - 1
		                                       = --------
		                                           z - 1

		   Now as z -> 1, our function -> 1/0 or infinity.
		   Unfortunately, L'Hopital's rule does not apply
		   in this case because it is no inter 0/0 */

		/* It has been noticed that the equation only blows up when in
		   regions of disinterest:  bottom frequency for high pass and
		   band pass, top frequency for low pass.  Since these are
		   regions of disinterest, it makes sense (as well as being
		   easy to implement) that the group delay be ignored when the
		   numerator is zero. */

		/* That's a good theory too, but it doesn't work either, the
		   graphs are screwy.  The thing to do is let gd be incremented
		   by 1 which is aproximately equal to what it generally is. */
		   
		if ( numerator.re == 0 && numerator.im == 0 ) {
/*		    if ( memcmp(type,"GD",2) == 0 )
			continue ;
*/		    gd++ ;
		}
		else
		    gd = gd + cmplxtof( cmplxdiv(pd,numerator) );

		a0 = Sd[iptr];
		a1 = Sd[iptr + 1];
		a2 = Sd[iptr + 2];

		temp1 = flttocmplx(a2,0.) ;
		temp2 = cmplxmul(temp1,z) ;
		temp3 = flttocmplx(a1,0.) ;
		temp4 = cmplxadd(temp2, temp3) ;
		temp5 = cmplxmul(temp4,z) ;
		temp6 = flttocmplx(a0,0.) ;
		denominator = cmplxadd(temp5, temp6);

		temp1 = flttocmplx(a2*2.,0.) ;
		temp2 = cmplxmul(temp1,z) ;
		temp3 = flttocmplx(a1,0.) ;
		temp4 = cmplxadd(temp2,temp3) ;
		pd = cmplxmul(temp4, z);

		/* if ( numerator.re != 0 || numerator.im != 0 ) */
		    gd = gd - cmplxtof( cmplxdiv(pd,denominator) );

		h = cmplxdiv(cmplxmul(h,numerator),denominator);

		iptr = iptr + 3;

	    }

	    if( memcmp(type,"AM",2) == 0 ){
		Response[j] = sqrt( cmplxtof( cmplxmul(cmplxcj( h ),h) ) );
	    }
	    else if( memcmp(type,"PH",2) == 0 ){
		phase = atan2( aimag( h ), cmplxtof( h ) );
		Response[j] = phase*180./pi;
	    }
	    else if( memcmp(type,"GD",2) == 0 ){
		Response[j] = gd*ts;
	    }

	    omega = omega + delomega;

	}

	return;
} /* end of function */


