#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ lrsmsp(nfreq, delfrq, xre, xim)
int nfreq;
double delfrq, xre[], xim[];
{
	int i, i_, npole, nzero;
	float const_;
	complexf pole[5], zero[3];

	complexf *const Pole = &pole[0] - 1;
	double *const Xim = &xim[0] - 1;
	double *const Xre = &xre[0] - 1;
	complexf *const Zero = &zero[0] - 1;




	/*   .....LRSM SP - Blacknest specified poles and zeros.....
	 * */


	/*   .....Set poles and zeros.....
	 *
	 *     const = 3.9927709 e3   <*  AMP of 1.0 at FREQ of 1.0 Hz */
	const_ = 3.9927709e3;
	nzero = 3;

	for( i = 1; i <= nzero; i++ ){
		i_ = i - 1;
		Zero[i] = flttocmplx( 0.0, 0.0 );
		}

	npole = 5;
	Pole[1] = flttocmplx( -5.60893, 7.40733 );
	Pole[2] = flttocmplx( -5.60893, -7.40733 );
	Pole[3] = flttocmplx( -9.27502, 0.0 );
	Pole[4] = flttocmplx( -28.24515, 14.97041 );
	Pole[5] = flttocmplx( -28.24515, -14.97041 );

	/*   .....Compute transfer function.....
	 * */
	getran( nfreq, delfrq, const_, nzero, zero, npole, pole, xre, 
	 xim );

	return;
} /* end of function */

