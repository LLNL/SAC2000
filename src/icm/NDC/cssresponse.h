
/*
 * SccsId:	@(#)include_src/cssresponse.h	106.1	05/15/96
 */


/*
 * Declarations for instrument response acquisition software 
 * Required by library, libresponse
 */

#include "complexNDC.h"

#ifndef _CSSRESPONSE_INCLUDE
#define _CSSRESPONSE_INCLUDE

typedef struct pzresp {	/* poles or zeros of poles and zeros response */
	int n;				/* number of zeros */
	DCOMPLEX *z,			/* pointer to complex values */
		 *e;			/* complex errors of values */
} PZRESP;

typedef struct polar {	/* polar coordinates */
	double a,			/* amplitude */
	       p;			/* phase */
} POLAR;

typedef struct faps  {	/* frequency, amplitude, phase response */
	int n;				/* number of frequencies */
	double *f,			/* pointer to frequencies */
	       *a,			/* pointer to amplitudes */
	       *p,			/* pointer to phases */
	       *ae,			/* pointer to amplitude errors */
	       *pe;			/* pointer to phase errors */
} FAPS;

typedef struct fir { /* finite impulse response */
    double isr;				/* input sample rate (sample/sec) */
    int    nnc,				/* number of numerator coefficients */
           ndc;				/* num of denominator coefficients */
    double *nu,				/* numerator coefficients */
	   *nue,			/* numerator coefficient errors */
           *de,				/* denuminator coefficients */
	   *dee;			/* denominator coefficient errors */
} FIR;

#endif /* !__CSSRESPONSE_INCLUDE */
