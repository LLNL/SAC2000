#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
void /*FUNCTION*/ getran(nfreq, delfrq, const_, nzero, zero, npole, 
	 pole, xre, xim)
int nfreq;
double delfrq, const_;
int nzero;
complexf zero[];
int npole;
complexf pole[];
double xre[], xim[];
{
	int idx, jdx ;
	double delomg, fac, omega, ti, ti0, tid, tin, tr, tr0, trd, trn;
	static float twopi = 6.283185307179586e0;

	complexf *const Pole = &pole[0] - 1;
	double *const Xim = &xim[0] - 1;
	double *const Xre = &xre[0] - 1;
	complexf *const Zero = &zero[0] - 1;



	/*   .....Subroutine to compute the transfer function.....
	 * */


	delomg = twopi*delfrq;

	for( jdx = 1; jdx <= nfreq; jdx++ ){
		omega = delomg*(double)( jdx - 1 );
		trn = 1.0e0;
		tin = 0.0e0;

		if( nzero != 0 ){
			for( idx = 1; idx <= nzero; idx++ ){
				tr = -cmplxtof( Zero[idx] );
				ti = omega - aimag( Zero[idx] );
				tr0 = trn*tr - tin*ti;
				ti0 = trn*ti + tin*tr;
				trn = tr0;
				tin = ti0;
			}
		}

		trd = 1.0e0;
		tid = 0.0e0;

		if( npole != 0 ){
			for( idx = 1; idx <= npole; idx++ ){
				tr = -cmplxtof( Pole[idx] );
				ti = omega - aimag( Pole[idx] );
				tr0 = trd*tr - tid*ti;
				ti0 = trd*ti + tid*tr;
				trd = tr0;
				tid = ti0;
			}
		}

		fac = (double)( const_ )/(powi(trd,2) + powi(tid,2));
		Xre[jdx] = fac*(trn*trd + tin*tid);
		Xim[jdx] = fac*(trd*tin - trn*tid);

		/* Output amplitude and phase when debugging */
		/*	       	if(delfrq*jdx>0.001 && delfrq*jdx<0.1) {
		  printf("%g %g %g\n", delfrq*jdx, sqrt(Xre[jdx]*Xre[jdx]+Xim[jdx]*Xim[jdx]), atan2(Xim[jdx],Xre[jdx])*360/twopi );
		  }*/


	}
	return;
} /* end of function */

