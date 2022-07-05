
/*
 * NAME
 *	fap -- Compute response using frequency, amplitude, phase triplets.

 * FILE
 *	fap.c

 * SYNOPSIS
 *	Determine an instrument response using frequency, amplitude, phase 
 *	triplets.

 * DESCRIPTION
 *	Function.  Using frequency, amplitude and phase triplets provided
 *	in a CSS instrument response file format, fap determines a response
 *	given the user input bounds.  The response curve can be either 
 *	"measured" or "theoretical", but is typically measured unless the
 *	frequency content requested is outside the bounds of the response
 *	file. 

 *	---- On entry ----
 *	nfr:		Number of frequency samples requested.
 *	log_flag: 	Plot in log (1) on linear (0) space.
 *	start_fr:	Starting frequency requested (Hz).
 *	end_fr:		Final frequency requested (Hz).
 *	faps:		Frequency, amplitude, phase response.

 *	---- On return ----
 *	result:		Unscaled response (amplitude and phase).

 *	---- Functions called ----
 *	Local
 *		lagrange: Lagrangian interpolation routine.

 * DIAGNOSTICS
 *	Note debug defines in code.

 * NOTES
 *	Theoretical curves should be used when extrapolating values 
 *	outside a given frequency range.

 * SEE ALSO
 *	Parent function, unscaled_response(3) and related routines using
 *	poles and zeros, paz(), and finite impulse responses, fir().

 * AUTHOR
 *	Mary Ann Brennan, Teledyne Geotech, 1991
 *		Created.
 *	Walt Nagy, SAIC, Mar. 18, 1992
 *		Added prologue.
 */

#ifndef	lint
static	char	SccsId[] = "@(#)libresponse/fap.c	105.1	05/29/96 Copyright 1994 Science Applications International Corporation.";
#endif


#ifndef NDEBUG
#define DBG(x) x
#else
#define DBG(x)
#endif /*!NDEBUG*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "css_general.h"
#include "csserrno.h"
#include "cssresponse.h"
#include "libresponse.h"


int fap (nfr, log_flag, start_fr, end_fr, faps, result)
int	nfr; 
int	log_flag;			/* number of result frequencies */
double	start_fr;			/* starting & ending frequency */
double	end_fr;				
FAPS	*faps;				/* freq, amp, phase response */
POLAR	*result;			/* result amplitude and phase */
{
	int	i, j, order;
	double	freq;			/* frequency or log(f) if log_flag */
	double	ftmp1;
	double	ftmp2;
	double	delta_f;		/* delta frequency or dlog(f) */
	double	*f, *a;			/* temporaries for log(f) log(a) */
    
#ifndef NDEBUG
	fprintf (stderr, "%s\n", "fap_freq.plot");
	for (i = 0; i < faps->n; i++)
	{
		fprintf (stderr, "%lf %lf %lf\n", faps->f[i], faps->a[i], 
			 faps->p[i]);
	}
	fprintf (stderr, "%s\n","end");
	fprintf (stderr, "%s\n", "fap_result.plot");
#endif /*!NDEBUG*/
    
	/* Allocate temporary arrays */

	if ((f = (double *)malloc(2*faps->n * sizeof(double))) == NULL)
	{
		return ERR;
	}
	a = &f[faps->n];		/* 1 malloc, 2 arrays */
    
	/* Lagrange interpolation works better for log function */
	for (i = 0; i < faps->n; i++)
	{
		if (faps->f[i] <= 1.0e-20)
			faps->f[i] = 1.0e-20;	/* To avoid log(0) */
		if (faps->a[i] <= 1.0e-20)
			faps->a[i] = 1.0e-20;	/* To avoid log(0) */
    
		f[i] = log10(faps->f[i]);	/* load temporary array */
		a[i] = log10(faps->a[i]);	/* load temporary array */
	}
	if (log_flag)
	{	/* constant delta in log(f) space */
		if (start_fr <= 1.0e-20)
			start_fr = 1.0e-20;	/* To avoid log(0) */
		if (end_fr <= 1.0e-20)
			end_fr = 1.0e-20;	/* To avoid log(0) */

		start_fr = log10((double) start_fr);	/* log(f0) */
		end_fr = log10((double) end_fr);	/* log(fe) */
	}
	if (nfr == 1)			/* avoid 0 in denominator */
		delta_f = 1.0;		/* value irrelevant, loop ends first */
	else
		delta_f = (end_fr - start_fr)/(nfr - 1.0);
    
	for (j = 0; j < nfr; j++)
	{
		if (log_flag)
		{	/* constant delta in log(f) space */
			freq = start_fr + j*delta_f; /* actually log freq */
		}
		else 
		{	/* constant delta in linear f space */
	    		/* lagrange interpolation works better for log func. */

			ftmp2 = start_fr + j*delta_f;
			if (ftmp2 <= 1.0e-20)
				ftmp2 = 1.0e-20;	/* To avoid log(0) */

			freq = log10(ftmp2);
		}
		result[j].p = 0.0;
		result[j].a = 1.0;
		for (i = 0; i < faps->n; i++)
		{
			if (freq < f[i])
				break;
		}
		i -= 2;
		order = 4;
		if (i < 0)
		{
			i = 0;
			order = 2;
		}
		else if (i > faps->n - 4)
		{
			i = faps->n - 2;
			order = 2;
		}
		lagrange (&a[i], &f[i], order, freq, &ftmp1);
		result[j].a = ftmp1;
		lagrange (&faps->p[i], &f[i], order, freq, &ftmp1);
		result[j].p = ftmp1;
		result[j].a = pow(10.0,result[j].a);
		DBG(fprintf (stderr, "%lf %lf %lf\n", freq,
			     result[i].a, result[i].p));
	}

	DBG(fprintf (stderr, "%s\n","end"));
    
	free (f);
	return OK;
}
