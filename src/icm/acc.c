#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ acc(nfreq, delfrq, xre, xim)
int nfreq;
double delfrq, xre[], xim[];
{
	int i, i_, npole, nzero;
	float const_;
	complexf pole[2], zero[2];

	complexf *const Pole = &pole[0] - 1;
	double *const Xim = &xim[0] - 1;
	double *const Xre = &xre[0] - 1;
	complexf *const Zero = &zero[0] - 1;





	/*   .....Acceleration Spectral Operator.....
	 *
	 *
	 *   .....Set poles and zeros.....
	 * */
	const_ = 1.0;
	npole = 0;
	nzero = 2;

	for( i = 1; i <= nzero; i++ ){
		i_ = i - 1;
		Zero[i] = flttocmplx( 0.0, 0.0 );
		}

	/*   .....Compute transfer function.....
	 * */
	getran( nfreq, delfrq, const_, nzero, zero, npole, pole, xre, 
	 xim );

	return;
} /* end of function */

