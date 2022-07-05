#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ llsn(nfreq, delfrq, xre, xim)
int nfreq;
double delfrq, xre[], xim[];
{
	int i, i_, npole, nzero;
	float const_;
	complexf pole[10], zero[5];

	complexf *const Pole = &pole[0] - 1;
	double *const Xim = &xim[0] - 1;
	double *const Xre = &xre[0] - 1;
	complexf *const Zero = &zero[0] - 1;




	/*   .....LLSN - for an L-4 seismometer.....
	 *     (poles and zeros due to P. Rodgers)
	 * */


	/*   .....Set poles and zeros.....
	 *
	 *     const = -3.57425 e15   <*  = -A * B */
	const_ = -3.57425e15;
	/*     nzero = 5              <* checked on 2/2/81 */
	nzero = 5;

	for( i = 1; i <= nzero; i++ ){
		i_ = i - 1;
		Zero[i] = flttocmplx( 0.0, 0.0 );
		}

	npole = 10;
	Pole[1] = flttocmplx( -5.026548, 3.769911 );
	Pole[2] = flttocmplx( -5.026548, -3.769911 );
	Pole[3] = flttocmplx( -0.5969026, 0.0 );
	Pole[4] = flttocmplx( -0.5969026, 0.0 );
	Pole[5] = flttocmplx( -276.460154, 0.0 );
	Pole[6] = flttocmplx( -276.460154, 0.0 );
	Pole[7] = flttocmplx( -376.99112, 0.0 );
	Pole[8] = flttocmplx( -376.99112, 0.0 );
	Pole[9] = flttocmplx( -571.76986, 583.32194 );
	Pole[10] = flttocmplx( -571.76986, -583.32194 );

	/*   .....Compute transfer function.....
	 * */
	getran( nfreq, delfrq, const_, nzero, zero, npole, pole, xre, 
	 xim );

	return;
} /* end of function */

