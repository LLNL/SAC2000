/*
 * Header file for complex operations.   November 24, 1987   TWM 
 * Required by library, libresponse
 */

/*
 * SccsId:	@(#)include_src/complex.h	106.1	05/15/96
 */

#ifndef _CSS_COMPLEX_INCLUDE
#define _CSS_COMPLEX_INCLUDE

#include <math.h>

#ifndef M_PI
#define M_PI 3.1415926536
#endif
#define TWOPI (2*M_PI)
#define RAD_TO_DEG (180./M_PI)
#define RAD 1
#define DEG 0

typedef struct {
	double          r, i;
	}               DCOMPLEX;

	DCOMPLEX        zload();
	DCOMPLEX        zadd();
	DCOMPLEX        zfadd();
	DCOMPLEX        zsubt();
	DCOMPLEX        zmult();
	DCOMPLEX        zfmult();
	DCOMPLEX        zdiv();
	DCOMPLEX        zconjg();
	DCOMPLEX        zexp();

	double          zabs();
	double          zphas();

#endif /* !_CSS_COMPLEX_INCLUDE */
