#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ lll(nfreq, delfrq, xre, xim, subtyp, subtyp_s, freepd, 
	 damp, nerr)
int nfreq;
double delfrq, xre[], xim[];
char *subtyp;   int subtyp_s;
double freepd, damp;
int *nerr;
{
	int i, i_, npole, nzero;
	float const_, h, om0, rad, t0;
	complexf pole[8], zero[3];
	static double twopi = 6.283185307179586;

	complexf *const Pole = &pole[0] - 1;
	double *const Xim = &xim[0] - 1;
	double *const Xre = &xre[0] - 1;
	complexf *const Zero = &zero[0] - 1;




	/*   .....LLL - for all LLL broadband (analog) seismometers.....
	 * */




	/* LLL STATION NETWORK AT ELKO, KANAB, LANDERS, MINA.
	 * THIS SECTION CONSISTS OF THE INSTRUMENT RESPONSE AS WELL AS THE IW
	 * TERM TO CONVERT DISPLACEMENT INTO VELOCITY.
	 * THE TRANSFER FUNCTION IS SHIFTED BY PI FROM THE PUBLISHED TRANSFER
	 * FUNCTION IN ORDER TO CONFORM WITH SIGN CONVENTIONS.
	 * REFERENCES ARE:
	 *     JUDD, D. D. (1969), DETERMINATION OF TRANSFER FUNCTION OF
	 *        SEISMOMETER/TELEMETRY SYSTEM THROUGH USE OF PSEUDO-RANDOM
	 *        BINARY SEQUENCES, UCRL-50490.
	 *     DENNY, M. D. (1977), THE INSTALLATION OF HORIZONTAL SEISMOMETERS
	 *        IN THE LLL SEISMIC NET AND THEIR CALIBRATION, UCRL-52216.
	 * */
	if( memcmp(subtyp,"LV      ",8) == 0 ){
		t0 = 20.9;
		h = 0.800;
		}
	else if( memcmp(subtyp,"LR      ",8) == 0 ){
		t0 = 39.2;
		h = 0.707;
		}
	else if( memcmp(subtyp,"LT      ",8) == 0 ){
		t0 = 39.6;
		h = 0.707;
		}
	else if( memcmp(subtyp,"MV      ",8) == 0 ){
		t0 = 19.3;
		h = 0.790;
		}
	else if( memcmp(subtyp,"MR      ",8) == 0 ){
		t0 = 41.5;
		h = 0.707;
		}
	else if( memcmp(subtyp,"MT      ",8) == 0 ){
		t0 = 40.6;
		h = 0.707;
		}
	else if( memcmp(subtyp,"KV      ",8) == 0 ){
		t0 = 20.0;
		h = .800;
		}
	else if( memcmp(subtyp,"KR      ",8) == 0 ){
		t0 = 40.56;
		h = 0.707;
		}
	else if( memcmp(subtyp,"KT      ",8) == 0 ){
		t0 = 39.98;
		h = 0.707;
		}
	else if( memcmp(subtyp,"EV      ",8) == 0 ){
		t0 = 30.9;
		h = 0.770;
		}
	else if( memcmp(subtyp,"ER      ",8) == 0 ){
		t0 = 40.4;
		h = 0.707;
		}
	else if( memcmp(subtyp,"ET      ",8) == 0 ){
		t0 = 40.4;
		h = 0.707;
		}
	else if( memcmp(subtyp,"BB      ",8) == 0 ){
		t0 = freepd;
		h = damp;
		}
	else{
		*nerr = 2105;
		setmsg( "ERROR", *nerr );
		apcmsg( "LLL:",5 );
		apcmsg( subtyp,subtyp_s );
		goto L_8888;
		}


	om0 = twopi/t0;
	const_ = 3.93785011e12;
	nzero = 3;
	for( i = 1; i <= nzero; i++ ){
		i_ = i - 1;
		Zero[1] = flttocmplx( 0.0, 0.0 );
		}
	npole = 8;
	/*    ??????????????????????????????? */
	rad = sqrt( 1.0 - powi(h,2) );
	Pole[1] = flttocmplx( -om0*h, om0*rad );
	Pole[2] = flttocmplx( -om0*h, -om0*rad );
	/* ???????????????????????????????????? */
	Pole[3] = flttocmplx( -114.28, 23.317 );
	Pole[4] = flttocmplx( -114.28, -23.317 );
	Pole[5] = flttocmplx( -100.48, 70.65 );
	Pole[6] = flttocmplx( -100.48, -70.65 );
	Pole[7] = flttocmplx( -67.677, 120.85 );
	Pole[8] = flttocmplx( -67.677, -120.85 );

	/*   .....Compute transfer function.....
	 * */
	getran( nfreq, delfrq, const_, nzero, zero, npole, pole, xre, 
	 xim );

L_8888:
	return;
} /* end of function */

