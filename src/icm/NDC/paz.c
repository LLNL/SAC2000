
/*
 * NAME
 *	paz -- Compute response using poles and zeros.

 * FILE
 *	paz.c

 * SYNOPSIS
 *	Determine an instrument response using poles and zeros.

 * DESCRIPTION
 *	Function.  Using response data composed of poles and zeros in a
 *	CSS instrument response file format, paz determines a response
 *	given the user input bounds.  The response curve will be a
 *	a "theoretical" response determined for a given instrument.

 *	---- On entry ----
 *	nfr:		Number of frequency samples requested.
 *	log_flag: 	Plot in log (1) on linear (0) space.
 *	start_fr:	Starting frequency requested (Hz).
 *	end_fr:		Final frequency requested (Hz).
 *	poles:		Poles of instrument response function.
 *	zeros:		Zeros of instrument response function.

 *	---- On return ----
 *	result:		Unscaled response (amplitude and phase).

 *	---- Functions called ----
 *	Local
 *		topolar: Convert from complex to polar coordinates.

 * DIAGNOSTICS
 *	None.

 * NOTES
 *	None.

 * SEE ALSO
 *	Parent function, unscaled_response(3) and related routines using
 *	frequency, amplitude, phase triplets, paz(), and finite impulse 
 *	responses, fir().

 * AUTHOR
 *	Mary Ann Brennan, Teledyne Geotech, 1991
 *		Created.
 *	Walt Nagy, SAIC, Mar. 18, 1992
 *		Added prologue.
 */


#ifndef	lint
static	char	SccsId[] = "@(#)libresponse/paz.c	105.1	05/29/96 Copyright 1994 Science Applications International Corporation.";
#endif

#include <stdio.h>
#include <math.h>
#include "complexNDC.h"
#include "cssresponse.h"
#include "libresponse.h"

#ifndef TWOPI
#define	TWOPI	6.283185308
#endif 

/* Compute response using poles and zeros */
void
paz (nfr, log_flag, start_fr, end_fr, poles, zeros, result)
int	nfr;
int	log_flag;		/* nfr- number of result frequencies */
double	start_fr;		/* starting & ending frequency */
double	end_fr;	
PZRESP	*poles;			/* poles of response function */
PZRESP	*zeros;			/* zeros of response function */
POLAR	*result;		/* result amplitude and phase */
{
	int	i, j;
	double	omega, amp, phase, delta, delta_f;
	
	if (nfr == 1)			/* avoid 0 in denominator */
		delta_f = 1.0;		/* value irrelevant, loop ends first */
	else if (log_flag)
	{				/* constant delta in log(f) space */
		/* where log(d) = (log(fe) - log(f0))/(nfr - 1) is constant */
		if (start_fr <= 1.0e-20)
			start_fr = 1.0e-20;	/* To avoid log(0) */
		if (end_fr <= 1.0e-20)
			end_fr = 1.0e-20;	/* To avoid log(0) */

		delta_f = pow(10.0, (log10((double)end_fr) 
				     -log10((double)start_fr))/(nfr - 1.0));
	}
	else				/* constant linear spacing */
		delta_f = (end_fr - start_fr)/(nfr - 1.0);
    
	for (j = 0, delta = 1; j < nfr; j++, /* delta_f^(j>0) is */ 
	     delta *= delta_f)
	{
		if (log_flag)
			/* log(f[j]) is log(f0) + j*log(d) is log(f0*d^j), 
			 * d is delta_f */
			omega = TWOPI * start_fr * delta;
		else
			omega = (double)(TWOPI * (start_fr + j*delta_f));
		result[j].p = 0.0;
		result[j].a = 1.0;
		for ( i = 0; i < zeros->n; i++)
		{
			topolar(-zeros->z[i].r,(omega - zeros->z[i].i),
				&amp,&phase);
			result[j].a *= amp;
			result[j].p += phase;
		}
		for ( i = 0; i < poles->n; i++)
		{
			topolar(-poles->z[i].r,(omega - poles->z[i].i),
				&amp,&phase);
			if (amp != 0.0) result[j].a /= amp;
			result[j].p -= phase;
		}
	}
}

