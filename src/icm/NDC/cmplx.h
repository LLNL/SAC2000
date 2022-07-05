/*
 * Copyright 1993 Science Applications International Corporation.
 */

/*
 * NAME
 * 
 * FILE 
 *
 * SYNOPSIS
 *
 * DESCRIPTION
 *
 * DIAGNOSTICS
 *
 * FILES
 *
 * NOTES
 * 
 * SEE ALSO
 *
 * AUTHOR
 *
 */

#ifndef COMPLEX_H
#define COMPLEX_H

#ifndef	lint
static	char	SccsId_cmplx_h[] = "@(#)libwav/cmplx.h	116.1 07/10/97 Copyright 1993 Science Applications International Corporation.";
#endif



#include <math.h>
#include "cdefs.h"

typedef	struct 
{
	double	r;
	double	i;
} complex;

complex ___t___;

#define CSET(x,y)     (___t___.r = (x), ___t___.i = (y),  ___t___)
#define CEQ(u,v)      (___t___.r = (u).r, ___t___.i = (u).i, ___t___)
#define CONJ(u)       (___t___.r = (u).r, ___t___.i = -(u).i, ___t___)
#define CMUL(u,v)    (___t___.r = (u).r * (v).r - (u).i * (v).i, \
                       ___t___.i = (u).i * (v).r + (u).r * (v).i, ___t___)
#define CSQ(u)        ((u).r * (u).r + (u).i * (u).i)
#define CADD(u,v)   (___t___.r = (u).r + (v).r, \
                       ___t___.i = (u).i + (v).i, ___t___)
#define CSUB(u,v)   (___t___.r = (u).r - (v).r, \
                       ___t___.i = (u).i - (v).i, ___t___)


Proto (extern complex, cset, (double, double));
Proto (extern complex, cadd, (complex, complex));
Proto (extern complex, csub, (complex, complex));
Proto (extern complex, cmult, (complex, complex));
Proto (extern complex, cdiv, (complex, complex));
Proto (extern complex, conj, (complex));
Proto (extern double, cabs, (complex));
Proto (extern complex, csqrt, (complex));
Proto (extern complex, rcmult, (double, complex));
Proto (extern double, cmult_real, (complex, complex));
Proto (extern double, cmult_imag, (complex, complex));


#endif /* COMPLEX_H */
