/*
 *  Copyright 1994 Science Applications International Corporation.
 *
 *
 *  libresponse.h include file.
 *	
 *  SCCSID: @(#)libresponse/libresponse.h	105.1	05/29/96
 */

#ifndef	LIBRESPONSE_H
#define	LIBRESPONSE_H

#include <stdio.h>

#ifndef _CDEFS_H_
#include "cdefs.h"	/* For Proto macro */
#endif

#ifndef _CSSRESPONSE_INCLUDE
#include "cssresponse.h"	/* For POLAR, FAPS, FIR, PZRESP types */
#endif 

#ifndef _CSS_COMPLEX_INCLUDE
#include "complexNDC.h"		/* For DCOMPLEX type */
#endif



/* from csserror.c */
extern char *cssgeterror(int error_code); 
extern void setcsserrno(int code);
extern void csserror(char *s, int t);

/* from fap.c */
extern int fap(int nfr, int log_flag,
			 double start_fr, double end_fr,
			 FAPS *faps, POLAR *result);


/* from fir.c */
extern int fir(int nfr, int log_flag,
			 double start_fr, double end_fr,
			 FIR *firs, POLAR *result);

/* from lagrange.c */
extern void lagrange(double *f, double *xi, int n, double x, 
			       double *fx);

/* from odfftr.c */

extern void odfftr ( int* nexp, float* xr, int flag);
/* from paz.c */
extern void paz(int nfr, int log_flag,
			  double start_fr, double end_fr,
			  PZRESP *poles, PZRESP *zeros,
			  POLAR *result);


/* from polar.c */
extern void topolar(double real, double imag, double *amp, 
			      double *phase);
extern void tocmplx(double *real, double *imag, double amp, 
			      double phase);

/* from scaled_response.c */
extern int  scaled_response(char *dir, char *file, char *type,
				      int units, int log_flag,
				      double start_fr, double end_fr, int nfr,
				      double calib, double calper,
				      DCOMPLEX *response);


/* from unscaled_response.c */
extern int unscaled_response(char *dir, char *file, char *type,
				       int units, int log_flag,
				       double start_fr, double end_fr, 
				       int nfr, DCOMPLEX *response);

extern int find_group(FILE *fp, char *rtype, char *form);


/* from inst_resp.c */
extern int get_inst_resp(char *sta, char *chan, double time, 
				   char *sensor_table, char *instrument_table,
				   char *type, int units, double flo, 
				   double fhi,int nfreq,DCOMPLEX **response);



#endif /* LIBRESPONSE_H */
