#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "coda.h"



void C_wigint(float* x, float* y, int npts, double dx, double eps, double t, float* f)
{
	int j, j_, n1;
	float a, am, amd, amu, dxd, dxj, dxj1, dxj1s, dxjs, dxu, dy, dyd, 
	 dyu, epsi, h, hc, hs, sp, sp1, t1, t2, t3, t4, w, wd, wu;

	float *const X = &x[0] - 1;
	float *const Y = &y[0] - 1;


	/*=====================================================================
	 * PURPOSE:  Interpolates evenly or unevenly spaced data.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    X:       X array if unevenly spaced, first x if evenly spaced. [f]
	 *    Y:       Y array. [fa]
	 *    NPTS:    Length of (X and) Y arrays. [i]
	 *    DX:      Set to 0.0 if unevenly spaced, to sampling interval
	 *             if evenly spaced. [f]
	 *    EPS:     Interpolation factor. [f]
	 *    T:       Time value to interpolate to. [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    F:       Interpolated y value. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  SCM/4
	 *=====================================================================
	 * REFERENCE: Wiggins, 1976, BSSA, 66, p.2077.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970202:  Modified order of input arguments.
	 *    86xxxx:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870202 (Prolog only.)
	 *===================================================================== */
	epsi = .0001;
	if( eps > 0. )
		epsi = eps;
	if( dx == 0. )
		goto L_10;
	j = (int)( (t - X[1])/dx );
	dxj = t - X[1] - (float)( j )*dx;
	j = j + 1;
	if( dxj == 0. )
		goto L_99;
	h = dx;
	dxj1 = dxj - h;
	dxd = h;
	dxu = h;
	goto L_50;
L_10:
	for( j = 1; j <= npts; j++ ){
		j_ = j - 1;
		a = X[j] - t;
		if( a > 0. )
			goto L_30;
		}
L_30:
	j = j - 1;
	dxj = t - X[j];
	if( dxj == 0. )
		goto L_99;
	h = X[j + 1] - X[j];
	dxj1 = t - X[j + 1];
L_50:
	hs = h*h;
	hc = hs*h;
	dxjs = dxj*dxj;
	dxj1s = dxj1*dxj1;
	dy = Y[j + 1] - Y[j];
	am = dy/h;
	amd = am;
	amu = am;
	if( j == 1 )
		goto L_60;
	if( dx != 0. ){
		dxd = dx;
		}
	else{
		dxd = X[j] - X[j - 1];
		}
	dyd = Y[j] - Y[j - 1];
	amd = dyd/dxd;
L_60:
	n1 = j + 1;
	if( n1 == npts )
		goto L_70;
	if( dx != 0. ){
		dxu = dx;
		}
	else{
		dxu = X[j + 2] - X[j + 1];
		}
	dyu = Y[j + 2] - Y[j + 1];
	amu = dyu/dxu;
L_70:
        wd = 1./epsi;
        if(fabs(amd) >= epsi) wd = 1./fabs(amd);
        w = 1./epsi;
        if(fabs(am) >= epsi) w = 1./fabs(am);
        wu = 1./epsi;
        if(fabs(amu) >= epsi) wu = 1./fabs(amu);
	sp = (wd*amd + w*am)/(wd + w);
	sp1 = (w*am + wu*amu)/(w + wu);
	t1 = Y[j]*(dxj1s/hs + 2.*dxj*dxj1s/hc);
	t2 = Y[j + 1]*(dxjs/hs - 2.*dxj1*dxjs/hc);
	t3 = sp*dxj*dxj1s/hs;
	t4 = sp1*dxjs*dxj1/hs;
	*f = t1 + t2 + t3 + t4;
	goto L_100;
L_99:
	*f = Y[j];
L_100:
	return;
} /* end of function */

