#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ oldkir(nfreq, delfrq, xre, xim)
int nfreq;
double delfrq, xre[], xim[];
{
	int i, i_, npole, nzero;
	float const_;
	complexf pole[4], zero[3];

	complexf *const Pole = &pole[0] - 1;
	double *const Xim = &xim[0] - 1;
	double *const Xre = &xre[0] - 1;
	complexf *const Zero = &zero[0] - 1;




	/*   .....OLD KIRN - Blacknest specified poles and zeros.....
	 * */


	/*   .....Set poles and zeros.....
	 *
	 *     const = 8.033295 e1   <*  AMP of 1.0 at FREQ of 1.0 Hz */
	const_ = 8.033295e1;
	nzero = 3;

	for( i = 1; i <= nzero; i++ ){
		i_ = i - 1;
		Zero[i] = flttocmplx( 0.0, 0.0 );
		}

	npole = 4;
	Pole[1] = flttocmplx( -0.2140, 0.2300 );
	Pole[2] = flttocmplx( -0.2140, -0.2300 );
	Pole[3] = flttocmplx( -0.3150, 0.0 );
	Pole[4] = flttocmplx( -80.0000, 0.0 );

	/*   .....Compute transfer function.....
	 * */
	getran( nfreq, delfrq, const_, nzero, zero, npole, pole, xre, 
	 xim );

	return;
} /* end of function */

