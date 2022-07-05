/*
 * NAME
 *	scaled_response -- Find a scaled instrument response curve.

 * FILE
 *	scaled_response.c

 * SYNOPSIS
 *	Extract a CSS formatted scaled instrument response curve.

 * DESCRIPTION
 *	Function.  This high level routine determines the appropriate 
 *	scaled instrument response curve formatted as prescribed by the 
 *	Center for Seismic Studies (CSS).  The response curve can be
 *	either "measured" or "theoretical" and cascaded from any 
 *	combination of these formatted types: frequency, amplitude,
 *	phase (fap) triplets; finite impulse responses (fir) filters; 
 *	and/or poles and zeroes (paz).  The first of these is usually 
 *	a measured response, while the latter two define theoretical 
 *	response curves.

 *	---- On entry ----
 *	dir:		Directory location of the instrument response file.
 *	file:		Instrument response file name.
 *	type:		Instrument response type, "measured" or "theoretical".
 *	units:		Instrument response measure, displacement ("d"), 
 *			velocity ("v") or acceleration ("a").
 *	log_flag: 	Plot in log (1) on linear (0) space.
 *	start_fr:	Starting frequency requested (Hz).
 *	end_fr:		Final frequency requested (Hz).
 *	nfr:		Number of frequency samples requested.
 *	calib:		Calibration factor -- Maps digital data to earth
 *			displacements (nm/digital count).
 *	calper:		Calibration period (sec).

 *	---- On return ----
 *	response:	Scaled response (in complex form).

 *	---- Functions called ----
 *	Local
 *		unscaled_response:	Find an unscaled response curve.

 * DIAGNOSTICS
 *	Retrun error status in variable, err.

 * NOTES
 *	In the future other group function may be added including, the
 *	SEED generic (gen), polynomial (pol), butterworth (but), and
 *	harmonic oscilator (har) formats.  Theoretical curves should be
 *	used when extrapolating values outside a given frequency range.

 * SEE ALSO
 *	Related function, unscaled_response(3) and instument response
 *	plotting routine, plot_ap(1).

 * AUTHOR
 *	Mary Ann Brennan, Teledyne Geotech, 1991
 *		Created.
 *	Walt Nagy, SAIC, Mar. 18, 1992
 *		Added prologue.
 */

#ifndef	lint
static	char	SccsId[] = "@(#)libresponse/scaled_response.c	105.1	05/29/96 Copyright 1994 Science Applications International Corporation.";
#endif


#include <stdio.h>
#include <math.h>
#include "csserrno.h"
#include "complexNDC.h"
#include "css_general.h"
#include "cssresponse.h"
#include "libresponse.h"

int
scaled_response (dir, file, type, units, log_flag, start_fr, 
		 end_fr, nfr, calib, calper,  response)
char	*dir;
char	*file;
char	*type;
int	units;
int	nfr, log_flag;
double	start_fr, end_fr;
double	calib, calper;
DCOMPLEX *response;
{
	double	scale;		/* amplitude scale factor */
	int	i;		/* loop counter */
	int	err;		/* error status */

	if ((err = unscaled_response (dir, file, type, 'd', log_flag, 
				      1.0/calper, 1.0/calper, 1, response)))
		return err;

    	/* scale = calib / amplitude of response at calper */

	scale = 1.0/(calib * sqrt(response[0].r * response[0].r
		    		+ response[0].i * response[0].i));

	if ((err = unscaled_response (dir, file, type, units, log_flag, 
				      start_fr, end_fr, nfr, response)))
		return err;

	for (i = 0; i < nfr; i++)
	{
		response[i].r *= scale;
		response[i].i *= scale;
	}
	return OK;
}
