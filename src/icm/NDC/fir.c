/*
 * NAME
 *	fir -- Compute response given a finite impulse response.
 *
 * FILE
 *	fir.c
 *
 * SYNOPSIS
 *	Determine an instrument response using a finite impulse response. 
 *
 * DESCRIPTION
 *	Function.  Using a pre-determined theoretical finite impulse 
 *	response in a CSS instrument response file format, fir determines 
 *	a response given the user input bounds.
 *
 *	---- On entry ----
 *	nfr:		Number of frequency samples requested.
 *	log_flag: 	Plot in log (1) on linear (0) space.
 *	start_fr:	Starting frequency requested (Hz).
 *	end_fr:		Final frequency requested (Hz).
 *	firs:		Finite impulse response.
 *
 *	---- On return ----
 *	result:		Unscaled group response (amplitude and phase).
 *
 *	---- Functions called ----
 *	Local
 *		fap:	Determine a response using freq., amp., phase data.
 *		ndfftr:	Perform a 1-D FFT.
 *
 * DIAGNOSTICS
 *	Note debug defines in code.
 *
 * NOTES
 *	None.
 *
 * SEE ALSO
 *	Parent function, unscaled_response(3) and related routines using
 *	poles and zeros, paz(), and frequency, amplitude, phase triplets,
 *	fap().
 *
 * AUTHOR
 *	Mary Ann Brennan, Teledyne Geotech, 1991
 *		Created.
 *	Walt Nagy, SAIC, Mar. 18, 1992
 *		Added prologue.
 */


#ifndef	lint
static	char	SccsId[] = "@(#)libresponse/fir.c	105.1	05/29/96 Copyright 1994 Science Applications International Corporation.";
#endif


#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "css_general.h"
#include "complexNDC.h"
#include "cssresponse.h"
#include "libresponse.h"

#ifndef TWOPI
#define	TWOPI	6.283185308
#endif

#define SIGN(a)  ((a<0.0) ? (-1) : (1))

int 
fir (nfr, log_flag, start_fr, end_fr, firs, result)
int	nfr;
int	log_flag;		/* nfr- number of result frequencies */
double	start_fr;		/* starting and ending frequency */
double	end_fr;
FIR	*firs;			/* fir parameters */
POLAR	*result;		/* group response result amplitude and phase */
{
	int	i, j, nexp, status, min, minindex = 0; 
	int flag = 1;  /* 1= forward transform, -1 reverse */
	float	*xr=NULL, isign, df;
	FAPS	faps;		/* frequency amplitude phase */
	double	*deriv = NULL;
    
	/* Initialize for error handling, one return at bailout */

	status = OK;
	faps.f = NULL;
    
	/* Allocate space */

	faps.n = 513;
	if ((xr = (float *)malloc((faps.n * 2) * sizeof(float))) == NULL)
	{
		status = ERR;
		goto bailout;
	}
	if ((faps.f = (double *)malloc(5 * faps.n * sizeof(double))) == NULL)
	{
		status = ERR;
		goto bailout;
	}
	faps.a = &faps.f[faps.n];	/* 1 malloc, 5 arrays */
	faps.p = &faps.f[2*faps.n];
	faps.ae = &faps.f[3*faps.n];
	faps.pe = &faps.f[4*faps.n];
	if ((deriv = (double *)malloc(faps.n * sizeof(double))) == NULL)
	{
		status = ERR;
		goto bailout;
	}
    
	/* Set up data in a large array */

	for (i = 0; i < firs->nnc; i++)
		xr[i] = firs->nu[i];
	for (i = firs->nnc; i < faps.n * 2; i++)
		xr[i] = 0.0;
    
	/* Set up and call fft for numerator coefficients */

	df = 1.0;
	/*isign = -1.0;*/
    
	/* Find greatest power of 2 that is less than twice faps.n */ 
	for (nexp = 1; pow((double)2, (double)nexp) < 2 * faps.n; nexp++)
		;

	nexp--;
    
#ifndef NDEBUG
	fprintf(stderr, "%s\n","xr.plot");
	for (i = 0; i < faps.n * 2; i++)
	{
		fprintf(stderr, "%d %f\n", i, xr[i]);
	}
	fprintf(stderr, "%s\n","end");
#endif /*!NDEBUG*/
    
	odfftr(&nexp,xr,flag);
    
#ifndef NDEBUG
	fprintf(stderr, "%s\n","xrfft.plot");
	for (i = 0; i < faps.n * 2; i++)
	{
		fprintf(stderr, "%d %f\n", i, xr[i]);
	}
	fprintf(stderr, "%s\n","end");
#endif /*!NDEBUG*/
    
	/* Compute frequency, amplitude, and phase for numerator (fir) */

	df = firs->isr/((faps.n - 1) * 2);
 /*       {
            int n = firs->nnc;
            double dt = 1.0 / firs->isr;
            double pi = 3.141593;
            double f = 0;
            double nexp;
            double cnexp;
            double snexp;
            double re;
            double im;
            double tmpre;
            double tmpim;
            for( i = 0; i < faps.n; ++i ){
               f = i * df;    
               nexp = (2*pi*dt*(n-1)/2)*f; 
               cnexp = cos(nexp);
               snexp = sin(nexp);
               re = xr[i*2];
               im = xr[i*2+1];
               tmpre = cnexp * re - snexp * im;
               tmpim = cnexp * im + snexp * re;
               xr[i*2] = tmpre; 
               xr[i*2+1] = tmpim; 
            }
            
            
            
            
            
        }
  */          
	
	
	for (i = 0; i < faps.n; i++)
	{
		faps.a[i] = hypot((double)xr[i*2],(double)xr[i*2+1]);
		if (xr[i*2] == 0 && xr[i*2+1] == 0)
		{
			faps.p[i] = 0;
		}
		else
		{
			faps.p[i] = atan2((double)xr[i*2],(double)xr[i*2+1]);
		}
		faps.f[i] = i * df;
	}
    
#ifndef NDEBUG
	fprintf(stderr, "%s\n", "freq_amp.plot");
	for (i = 0; i < faps.n; i++)
	{
		fprintf(stderr, "%lf %lf\n", faps.f[i], faps.a[i]);
	}
	fprintf(stderr, "%s\n","end");
	fprintf(stderr, "%s\n", "freq_phase.plot");
	for (i = 0; i < faps.n; i++) 
	{
		fprintf(stderr, "%lf %lf\n", faps.f[i], faps.p[i]);
	}
	fprintf(stderr, "%s\n","end");
#endif /*!NDEBUG*/
    
	if(firs->ndc > 0)
	{
		/* Set up data in a large array */
		for (i = 0; i < firs->ndc; i++)
			xr[i] = firs->de[i];
		for (i = firs->ndc; i < faps.n * 2; i++)
			xr[i] = 0.0;
	
		/* Set up and call fft for numerator coefficients */

		df = 1.0;
		odfftr(&nexp,xr,flag);
	
		/* Compute frequency, amplitude, and phase for denom. (iir) */

		for (i = 0; i < faps.n; i++)
		{
			faps.a[i] /= hypot((double)(1.0-xr[i*2]),
					   (double)xr[i*2+1]);
			if ((1.0 - xr[i*2]) == 0 && xr[i*2+1] == 0)
			{
				faps.p[i] -= 0;
			}
			else
			{
				faps.p[i] -= atan2((double)(1.0-xr[i*2]),
						   (double)xr[i*2+1]);
			}
		}
	}
    
	/* Make the phase a smooth function, i.e. get rid of wraps \/\ */
    
	/* Find the index with the derivative closest to zero */

	min = TWOPI;
	for (i = 0; i < faps.n - 1; i++)
	{
		deriv[i] = faps.p[i+1] - faps.p[i];
		if (fabs(deriv[i]) < min)
		{
			min = fabs(deriv[i]);
			minindex = i;
		}
	}
    
	/* Correct phases with high indicies */

	for (i = minindex; i  < faps.n - 2; i++)
	{
		if (SIGN(deriv[i]) != SIGN(deriv[i+1])) 
		{
			for (j = i+2; j < faps.n; j++)
				faps.p[j] = faps.p[j] + SIGN(deriv[i])* TWOPI;
	    		deriv[i+1] = faps.p[i+2] - faps.p[i+1];
		}
	}
    
	/* Correct phases with low indicies */
	for (i = minindex; i > 0; i--)
	{
		if (SIGN(deriv[i]) != SIGN(deriv[i-1]))
		{
			for (j=i-1; j>=0; j--)
				faps.p[j] = faps.p[j] + SIGN(deriv[i])* TWOPI;
			deriv[i-1] = faps.p[i] - faps.p[i-1];
		}
	}
    
#ifndef NDEBUG
	fprintf(stderr, "%s\n", "unwrap_freq_phase.plot");
	for (i = 0; i < faps.n; i++)
	{
		fprintf(stderr, "%lf %lf\n", faps.f[i], faps.p[i]);
	}
	fprintf(stderr, "%s\n","end");
#endif /*!NDEBUG*/
    
	/* Call fap to interpolate for proper values */
    
	status = fap (nfr, log_flag, start_fr, end_fr, &faps, result);
   
	bailout:
	if (xr != NULL)
		free(xr);
	if (faps.f != NULL)
		free(faps.f);
	if (deriv != NULL)
		free(deriv);
	return status;
}
