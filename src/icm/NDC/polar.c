
#ifndef	lint
static	char	SccsId[] = "@(#)libresponse/polar.c	105.1	05/29/96 Copyright 1994 Science Applications International Corporation.";
#endif

/* Functions to convert from and to polar coordinates */

#include <math.h>
#include "libresponse.h"

void
topolar (real, imag, amp, phase)
double	real, imag, *amp, *phase;
{
	*amp = sqrt(real*real + imag*imag);
	if (imag == 0.0 && real == 0.0) 
		*phase = 0.0;
	else 
		*phase = atan2(imag, real);
}

void
tocmplx (real, imag, amp, phase)
double	*real, *imag, amp, phase;
{
	*real = amp * cos(phase);
	*imag = amp * sin(phase);
}
