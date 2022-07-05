
/*
 * NAME
 *	unscaled_response -- Find a unscaled instrument response curve.

 * FILE
 *	unscaled_response.c

 * SYNOPSIS
 *	Extract a CSS formatted unscaled instrument response curve.

 * DESCRIPTION
 *	Function.  This high level routine determines the appropriate 
 *	unscaled instrument response curve formatted as prescribed by the 
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

 *	---- On return ----
 *	response:	Unscaled response (in complex form).

 *	---- Functions called ----
 *	Local
 *		paz:		Compute a response using poles and zeros.
 *		fap:		Compute a response using frequency,
 *				amplitude, phase triplets.
 *		fir:		Compute a response using a finite impulse 
 *				response.
 *		topolar:	Convert from complex to polar coordinates
 *		find_group:	Determine if type is paz, fap, or fir.

 * DIAGNOSTICS
 *	Return error status through CSS error status defines in csserrno.h.

 * NOTES
 *	In the future other group function may be added including, the
 *	SEED generic (gen), polynomial (pol), butterworth (but), and
 *	harmonic oscilator (har) formats.  Theoretical curves should be
 *	used when extrapolating values outside a given frequency range.

 * SEE ALSO
 *	Related function, scaled_response(3) and instument response
 *	plotting routine, plot_ap(1).

 * AUTHOR
 *	Jerry Carter, Rondout Assoicates Inc., 1985
 *		Created.
 *	Jerry Carter, SAIC, 1991
 *		Modified.
 *	Mary Ann Brennan, Teledyne Geotech, 1991
 *		Modified.
 *	Walt Nagy, SAIC, Mar. 18, 1992
 *		Added prologue.
 */

#ifndef	lint
static	char	SccsId[] = "@(#)libresponse/unscaled_response.c	105.1	05/29/96 Copyright 1994 Science Applications International Corporation.";
#endif

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include "csserrno.h"
#include "complexNDC.h"
#include "css_general.h"
#include "cssresponse.h"
#include "libresponse.h"

#ifndef TWOPI
#define	TWOPI	6.283185308
#endif

#define LINE_LENGTH 200
/*
 *        JWG (7/29/92):  Macro UFREE taken from aesir.h to handle freeing of
 *                        heap memory
 */

#define UFREE(ptr)		do \
                                {	if (ptr)\
					{	(void) free ((char *) (ptr));\
							(ptr) = NULL;\
						} \
				} while (0)


int
unscaled_response (dir, file, type, units, log_flag, start_fr, end_fr, 
		   nfr, response)
char	*dir, *file, *type;
int	units;
int	nfr, log_flag;
double	start_fr, end_fr;
DCOMPLEX *response;
{
	int	i, j;			/* loop control */
	int	status;			/* error status of function */
	int	nz;			/* number of zeros */
	int	np;			/* number of poles */
	double	a0;			/* normalizing factor */
	double	omega;			/* angular frequency */
	double	amp;			/* amplitude for conversion */
	double	phase;			/* phase for conversion */
	float	delta_f = (float) 0;	/* delta frequency */
	float	delta;			/* delta_f^j for constant dlog(f) */
	POLAR	*result=NULL;		/* result in polar coordinates */
	POLAR	*cascade=NULL;		/* cascaded results */
	PZRESP	poles;			/* poles of response function */
	PZRESP	zeros;			/* zeros of response function */
	FAPS	faps;			/* frequency, amp, phase response */
	FIR	firs;			/* finite impulse response structure */
	char	path[101];		/* response file path */
	char	buf[LINE_LENGTH + 1];		/* line buffer */
	char	form[10];		/* response form: paz, fap, fir */
	FILE	*resp_ptr=NULL;		/* response file pointer */
    
	/* Inititialize for error handling */

	status	= OK;
	poles.z	= NULL;
	zeros.z	= NULL;
	faps.f	= NULL;
	firs.nu	= NULL;
	firs.de	= NULL;
    
	/* Open the response file */
    
	sprintf (path, "%s/%s", dir, file);
	if ((resp_ptr = fopen (path,"r")) == (FILE *)NULL)
	{
		fprintf (stderr,"Could not open the response file %s.\n", path);
		status = ERR;
		goto bailout;
	}
    
	/* Check input parameters */

	if (log_flag)
	{				/* even spacing for log plot */
		if (start_fr == 0.0)	/* would yield log(0) */
		{
		    setcsserrno (CSSEDOM);
		    status = ERR2;
		    goto bailout;
		}	
	}

	/* Allocate group response result amplitude and phase */

	if ((result = (POLAR *) malloc(nfr * sizeof(POLAR))) == NULL)
	{
		status = ERR;
		goto bailout;
	}
    
	/* Allocate cascaded response amplitude and phase */

	if ((cascade = (POLAR *) malloc(nfr * sizeof(POLAR)))  == NULL)
	{
		status = ERR;
		goto bailout;
	}
	for (j = 0; j < nfr; j++)
	{	/* Initialize cascade */
		cascade[j].a = 1.0;
		cascade[j].p = 0.0;
	}
    

	/* Find the appropriate response */
    
	if ((status = find_group (resp_ptr, type, form)))
		goto bailout;
    
	int hadResult = 0;
	do
	{
		if (strcmp (form, "paz") == 0 || strcmp( form, "PAZ" ) == 0 )
		{
			np = nz = 0;
	    
	 		/* Read in paz response parameters */

			if (fgets (buf, LINE_LENGTH, resp_ptr) == NULL)
			{
				setcsserrno (CSSESCAN);
				status = ERR2;
				goto bailout;
			}
			if (sscanf (buf, "%lf", &a0) != 1) 
			{
				setcsserrno (CSSESCAN);
				status = ERR2;
				goto bailout;
			}
			if (fgets (buf, LINE_LENGTH, resp_ptr) == NULL) 
			{
				setcsserrno (CSSESCAN);
				status = ERR2;
				goto bailout;
			}
			if (sscanf (buf, "%d", &np) != 1) 
			{
				setcsserrno (CSSESCAN);
				status = ERR2;
				goto bailout;
			}
			poles.n = np;
	    
			if (np)
			{	/* Skips malloc and read if no poles */
				if ((poles.z = (DCOMPLEX *)malloc(2 * np * 
				     sizeof(DCOMPLEX))) == NULL)
				{
					status = ERR;
					goto bailout;
				}
				poles.e = &poles.z[np];	/* 1 malloc, 2 arrays */
				for (i = 0; i < np; i++)
				{
					if (fgets (buf, LINE_LENGTH, resp_ptr) == NULL)
					{
						setcsserrno (CSSESCAN);
						status = ERR2;
						goto bailout;
					}
					if (sscanf (buf,"%lf %lf %lf %lf",
						   &poles.z[i].r, &poles.z[i].i,
						   &poles.e[i].r, &poles.e[i].i)
						   != 4)
					{
						setcsserrno (CSSESCAN);
						status = ERR2;
						goto bailout;
					}
				}
			}
			if (fgets (buf, LINE_LENGTH, resp_ptr) == NULL) 
			{
				setcsserrno (CSSESCAN);
				status = ERR2;
				goto bailout;
			}
			if (sscanf (buf, "%d", &nz) != 1)
			{
				setcsserrno (CSSESCAN);
				status = ERR2;
				goto bailout;
			}
			zeros.n = nz;
			if (nz)
			{	/* Skips malloc and read if no zeros */
				if ((zeros.z = (DCOMPLEX *)malloc(2 * nz * 
				     sizeof(DCOMPLEX))) == NULL)
				{
					status = ERR;
					goto bailout;
				}
				zeros.e = &zeros.z[nz];	/* 1 malloc, 2 arrays */
				for (i = 0; i < nz; i++)
				{
					if (fgets (buf, LINE_LENGTH, resp_ptr) == NULL)
					{
						setcsserrno (CSSESCAN);
						status = ERR2;
						goto bailout;
					}
					if (sscanf (buf,"%lf %lf %lf %lf",
					    &zeros.z[i].r, &zeros.z[i].i,
					    &zeros.e[i].r, &zeros.e[i].i) != 4)
					{
						setcsserrno (CSSESCAN);
						status = ERR2;
						goto bailout;
					}
				}
			}

			/* Compute response using poles and zeros */

			paz (nfr, log_flag, start_fr, end_fr, &poles, &zeros,
			     result);
		    
			for (j = 0; j < nfr; j++)
				result[j].a *= a0;
			if (np)
			{
				UFREE (poles.z);
			}
			if (nz)
			{
				UFREE (zeros.z);
			}
			int hadResult = 1;
		}	/* End of paz */

		else if (strcmp (form, "fap") == 0 || strcmp( form, "FAP" ) == 0 )
		{

	 		/* Read in fap response parameters */

			if (fgets (buf, LINE_LENGTH, resp_ptr) == NULL)
			{
				setcsserrno (CSSESCAN);
				status = ERR2;
				goto bailout;
			}
			if (sscanf (buf, "%d", &np) != 1)
			{
				setcsserrno (CSSESCAN);
				status = ERR2;
				goto bailout;
			}
			faps.n = np;
			if ((faps.f = (double *)malloc(5 * np * 
			     sizeof(double))) == NULL)
			{
				status = ERR;
				goto bailout;
			}
			faps.a  = &faps.f[np];		/* 1 malloc, 5 arrays */
			faps.p  = &faps.f[2*np];
			faps.ae = &faps.f[3*np];
			faps.pe = &faps.f[4*np];
			for (i = 0; i < np; i++)
			{
				if (fgets (buf, LINE_LENGTH, resp_ptr) == NULL)
				{
					setcsserrno (CSSESCAN);
					status = ERR2;
					goto bailout;
				}
				if (sscanf (buf, "%lf %lf %lf %lf %lf",
					    &faps.f[i], &faps.a[i], &faps.p[i],
					    &faps.ae[i], &faps.pe[i]) != 5)
				{
					setcsserrno (CSSESCAN);
					status = ERR2;
					goto bailout;
				}
				faps.p[i] *= TWOPI/360.0;
			}
		    
			/*
			 * Compute response using frequency, amplitude, 
			 * phase triplets
			 */

			if ((status = fap (nfr, log_flag, start_fr, end_fr,
				          &faps, result)))
				goto bailout;

			UFREE (faps.f);

			hadResult = 1;

		}	/* End of fap */

		else if (strcmp (form, "fir") == 0 || strcmp( form, "FIR" ) == 0 )
		{	    
			if (fgets (buf, LINE_LENGTH, resp_ptr) == NULL)
			{
				setcsserrno (CSSESCAN);
				status = ERR2;
				goto bailout;
			}
			if (sscanf (buf, "%lf", &firs.isr) != 1) 
			{	/* Input sample rate */
				setcsserrno (CSSESCAN);
				status = ERR2;
				goto bailout;
			}
		    
			if (fgets (buf, LINE_LENGTH, resp_ptr) == NULL) 
			{
				setcsserrno (CSSESCAN);
				status = ERR2;
				goto bailout;
			}
			if (sscanf (buf, "%d", &firs.nnc) != 1) 
			{ 	/* Num numerator coef */
				setcsserrno 	(CSSESCAN);
				status = ERR2;
				goto bailout;
			}
			if ((firs.nu = (double *)malloc(2 * firs.nnc * 
			     sizeof(double))) == NULL) 
			{
				status = ERR;
				goto bailout;
			}
			firs.nue = &firs.nu[firs.nnc];
			for (i = 0; i < firs.nnc; i++)
			{
				if (fgets (buf, LINE_LENGTH, resp_ptr) == NULL)
				{
					setcsserrno (CSSESCAN);
					status = ERR2;
					goto bailout;
				}
				if (sscanf (buf, "%lf %lf", &firs.nu[i],
					   &firs.nue[i]) != 2)
				{
					setcsserrno (CSSESCAN);
					status = ERR2;
					goto bailout;
				}
			}
		    
			if (fgets (buf, LINE_LENGTH, resp_ptr) == NULL)
			{
				setcsserrno (CSSESCAN);
				status = ERR2;
				goto bailout;
			}
			if (sscanf (buf, "%d", &firs.ndc) != 1)
			{	/* Num denominator coef */
				setcsserrno (CSSESCAN);
				status = ERR2;
				goto bailout;
			}
			if ((firs.de = (double *)malloc(2 * firs.ndc * 
					sizeof(double))) == NULL)
			{
				status = ERR;
				goto bailout;
			}
			firs.dee = &firs.de[firs.ndc];
			for (i = 0; i < firs.ndc; i++)
			{
				if (fgets (buf, LINE_LENGTH, resp_ptr) == NULL)
				{
					setcsserrno (CSSESCAN);
					status = ERR2;
					goto bailout;
				}
				if (sscanf (buf, "%lf %lf", &firs.de[i],
					   &firs.dee[i]) != 2)
				{
					setcsserrno (CSSESCAN);
					status = ERR2;
					goto bailout;
				}
			}
			if ((status = fir (nfr, log_flag, start_fr, end_fr,
				          &firs, result)))
				goto bailout;

			UFREE (firs.nu);
			UFREE (firs.de);

			hadResult = 1;

		}	/* End of fir */

		/* Cascade individual group responses */
		if (hadResult) { //Don't apply a filter if we didn't see one we recognized.
			for (j = 0; j < nfr; j++) {
				cascade[j].a *= result[j].a;
				cascade[j].p += result[j].p;
			}
		}
		hadResult = 0;
	} while (!find_group (resp_ptr, type, form));	/* End while groups */
    

	if (units != 'd')
	{	/* Must compute omega */
		if (nfr == 1)		/* avoid 0 in denominator */
			delta_f = 1.0;	/* value irrelevant, loop ends first */
		else if (log_flag)
		{	/* constant delta in log(f) space, where */
			/* log(d) = (log(fe) - log(f0))/(nfr - 1) is constant */
			if (start_fr <= 1.0e-20)
				start_fr = 1.0e-20;	/* To avoid log(0) */
			if (end_fr <= 1.0e-20)
				end_fr = 1.0e-20;	/* To avoid log(0) */

			delta_f = pow(10.0, (log10((double)end_fr) 
				  -log10((double)start_fr))/(nfr - 1.0));
		}
		else
			delta_f = (end_fr - start_fr)/(nfr - 1.0);
	}

	for (j = 0, delta = 1; j < nfr; j++, /* delta_f^(j>0) is */ 
	    delta *= delta_f)
	{	/*
		 * Divide by -omega**2 for acceleration, 
		 * i(omega) for velocity
		 */
		if (log_flag)
			/* log(f[j]) is log(f0) + j*log(d) is log(f0*d^j), 
			   d is delta_f */
			omega = TWOPI * start_fr * delta;
		else
			omega = (double)(TWOPI * (start_fr + j*delta_f));
	
		switch (units)
		{
			case 'a':
				topolar ((-omega*omega), 0.0, &amp, &phase);
				if (amp != 0.0) cascade[j].a /= amp;
				cascade[j].p -= phase;
				break;
			case 'v':
				topolar (0.0, omega, &amp, &phase);
				if (amp != 0.0) cascade[j].a /= amp;
				cascade[j].p -= phase;
				break;
		}
	
		tocmplx (&response[j].r, &response[j].i, cascade[j].a,
			 cascade[j].p);
	}
    
bailout:
	UFREE(poles.z);
	UFREE(zeros.z);
	UFREE(faps.f);
	UFREE(firs.nu);
	UFREE(firs.de);
	UFREE(result);
	UFREE(cascade);

	if (resp_ptr != NULL)
		fclose (resp_ptr);

	return (status);
}
int
find_group (fp, rtype, form)
FILE	*fp;
char	*rtype;		/* request type */
char	*form;		/* form found, paz, fap, fir, ... */
{
	char linebuff[LINE_LENGTH + 1];
	char type[20];
        const char *measured="measured";
        const char *theoretical="theoretical";
        const char *both="both";       
	clearerr (fp);
        while (fgets (linebuff, LINE_LENGTH, fp) != NULL)
		if ((*linebuff != '#') && (*linebuff != '\n'))
		{
                        sscanf (linebuff, "%s",type);

			/*"measured" or "theoretical"*/

                    
                        if (strstr (rtype, both)!=NULL)
                        {                 
                          
                            if ((strstr (linebuff, measured)!=NULL) || (strstr (linebuff, theoretical)!=NULL))
                            {                        
                                if (sscanf (linebuff, "%*s %*s %*s %s", form)
                                    != 1)
                                {
                                    setcsserrno (CSSESCAN);
				    return ERR2;
                                }
                                return OK;
                            }
                        }

			if (!strcmp (rtype, type))
			{
				if (sscanf (linebuff, "%*s %*s %*s %s", form)
				           != 1)
				{
					setcsserrno (CSSESCAN);
					return ERR2;
				}
				return OK;
			}
		}

	/* End of while */

	if (feof (fp))
	{
		setcsserrno (CSSEUNEOF);
		return ERR2;
	}
	return ERR;
}

int
find_group1 (fp, rtype, form)
FILE	*fp;
char	*rtype;		/* request type */
char	*form;		/* form found, paz, fap, fir, ... */
{
	char linebuff[LINE_LENGTH + 1];
	char type[20];
       
        
	clearerr (fp);
	while (fgets (linebuff, LINE_LENGTH, fp) != NULL)
		if ((*linebuff != '#') && (*linebuff != '\n'))
		{
                        sscanf (linebuff, "%s",type);
                        
			/*"measured" or "theoretical"*/
                        if (strcmp ("both", rtype))
                        {
                            if ((strcmp ("measured", type)) || (strcmp ("theoretical", type)))
                            {
                                if (sscanf (linebuff, "%*s %*s %*s %s", form) 
                                    != 1)
                                {
                                    setcsserrno (CSSESCAN);
				    return ERR2;
                                }
                                return OK;
                            }                           
                        }
                       
			if (!strcmp (rtype, type))
			{
				if (sscanf (linebuff, "%*s %*s %*s %s", form) 
				           != 1)
				{
					setcsserrno (CSSESCAN);
					return ERR2;
				}
				return OK;
			}
		}

	/* End of while */

	if (feof (fp))
	{
		setcsserrno (CSSEUNEOF);
		return ERR2;
	}
	return ERR;
}
