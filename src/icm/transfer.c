#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <strings.h>

#include "../../inc/hdr.h"
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/EVRESPnames.h"
#include "icm.h"

#include "../SeisMgr/cssListOps/dblPublicDefs.h"
#include "../SeisMgr/smDataIO.h"

void deblank ( char *strg );

int  GetCalibCalperFromWfdisc(DBlist tree, int wfid, double *calib, double* calper );
void getSensorInstrumentCalibInfo(double* ncalper, double* ncalib, double* calper, double* calratio );

int NearestInt( double D )
{
   double Floor = floor(D);
   double Ceil  = ceil(D);
   return (Ceil - D > D - Floor) ? (int) Floor : (int) Ceil;
}
/* ------------------------------------------------------------- */


int Unset( double value )
{
   return fabs( value + 999.0 ) < 0.001;
}
/* ------------------------------------------------------------- */



double CompareCalperValues( double WfdiscCalper, double ncalper, double SensorCalper )
{
   double tol = 0.001;
   double undef = -999.0;

   if( Unset(WfdiscCalper) && Unset(ncalper) && Unset(SensorCalper) )
      return undef;

   if( Unset(WfdiscCalper) && ! Unset(ncalper) ) {
      printf( "\tWfdisc Calper not set. Setting Wfdisc Calper to %f to match Nominal Calper\n", ncalper );
      WfdiscCalper = ncalper;
   }

   if( Unset(WfdiscCalper) && ! Unset(SensorCalper) ) {
      printf( "\tWfdisc Calper not set. Setting Wfdisc Calper to %f to match Sensor Calper\n", SensorCalper );
      WfdiscCalper = SensorCalper;
   }

   if( Unset(ncalper) && ! Unset(WfdiscCalper) ) {
      printf( "\tNominal Calper not set. Setting Nominal Calper to %f to match Wfdisc Calper\n", ncalper );
      ncalper = WfdiscCalper;
   }

   if( Unset(SensorCalper) && ! Unset(WfdiscCalper) ) {
      printf( "\tSensor Calper not set. Setting Sensor Calper to %f to match Wfdisc Calper\n", WfdiscCalper );
      SensorCalper = WfdiscCalper;
   }

   /* Now all calper values should be different from undef. If they differ, then
      the problem cannot be resolved. */


   if( fabs( WfdiscCalper - ncalper ) > tol ){
      printf( "\tWfdisc Calper differs from Instrument Calper. Values are: " );
      printf( "Wfdisc Calper = %f,   Instrument Calper = %f\n", WfdiscCalper, ncalper );
      printf( "\tUn-resolvable problem.\n" );
      return undef;
   }
   if( fabs( WfdiscCalper - SensorCalper ) > tol ){
      printf( "\tWfdisc Calper differs from Sensor Calper. Values are: " );
      printf( "Wfdisc Calper = %f,   Sensor Calper = %f\n", WfdiscCalper, SensorCalper );
      printf( "\tUn-resolvable problem.\n" );
      return undef;
   }
   if( fabs( ncalper - SensorCalper ) > tol ){
      printf( "\tNominal Calper differs from Sensor Calper. Values are: " );
      printf( "Nominal Calper = %f,   Sensor Calper = %f\n", ncalper, SensorCalper );
      printf( "\tUn-resolvable problem.\n" );
      return undef;
   }

   /* They are all the same, so return any one of them... */

   return ncalper;

}
/* ------------------------------------------------------------- */





int IsNormalized( double calper, int nfreq, double delfrq, const double* xre, const double* xim )
{
   double value;
   double NormFreq = 1.0 / calper;
   int j = NearestInt( NormFreq / delfrq );
   if( j < 0 || j >= nfreq ){
      printf( "\tCannot determine normalization status because NormFreq is outside bounds of transfer function.\n" );
      return 0;
   }
   value = sqrt( xre[j]*xre[j] + xim[j]*xim[j] );
   return fabs( value - 1.0 ) < 0.01;
}
/* ---------------------------------------------------------------------------------- */

int MeaningfulValue( double value )
{
   double tol = 0.001;
   return /*fabs( value - 1 ) > tol && */ fabs( value + 999 ) > tol;
}
/* ---------------------------------------------------------------------------------- */


double getEffectiveCalib( double calib, double ncalib, double calratio )
{
   double EffectiveCalib = 1.0;
   if( MeaningfulValue( calib ) ){
      EffectiveCalib *= calib;
      printf( "\tWfdisc calib value is (%f).\n", EffectiveCalib );
      if( MeaningfulValue( calratio ) ){
         EffectiveCalib *= calratio;
	 printf( "\tAfter multiplication by calratio of (%f), EffectiveCalib is (%f).\n",
	         calratio, EffectiveCalib );
      }
   }
   else if( MeaningfulValue( ncalib ) ){
      EffectiveCalib *= ncalib;
      printf( "\tEffectiveCalib is set to sensor.ncalib (%f).\n", ncalib );
   }

   return EffectiveCalib;
}
/* ---------------------------------------------------------------------------------- */



double GetNormalizationFactor( int nfreq, double delfrq, const double* xre, const double* xim, double *calper )
{
   /* This function attempts to determine a scaling factor to apply to the seismogram data
      to account for normalized vs non-normalized transfer functions, and scaled vs unscaled data.
      When the transfer function is non-normalized and the data were scaled on input, the scale
      factor is 1/calib.
      When the transfer function is normalized and the data have not been scaled on input, the
      scale factor is calib.
      In all other cases, the scale factor is 1.0.

      The transfer function is determined to be normalized when its value at the calper is within
      0.01 of 1.0.
      The data are determined not to have been scaled if the difference between calib and scale is < 0.01.
      This is because when data are scaled, SAC sets scale to 1.0, but leaves the calib value olone.
   */

   enum NormalizationStatus{ Normalized, UnNormalized, Unknown } NormStatus;
   enum ScalingStatus{ Scaled, UnScaled, ScaleUnknown } ScaleStatus;


   double DataMultiplier = 1.0;
   DBlist tree;          /* used in search for calib, calper */
   double calib; /* Used to determine whether data are scaled and
	                    whether response is normalized. */
   double ncalper, ncalib, SensorCalper, calratio;
   double EffectiveCalib;


   getSensorInstrumentCalibInfo( &ncalper, &ncalib, &SensorCalper, &calratio );


   tree = smGetDefaultTree();

   if( !GetCalibCalperFromWfdisc(tree, *nwfid, &calib, calper ) ){
      calib = -999.0;
      *calper = -999.0;
   }
   *calper = CompareCalperValues( *calper, ncalper, SensorCalper );

   /* Now determine whether response has been normalized or not... */
   if( *calper <= 0 ){
      NormStatus = Unknown;
      printf( "\tCalper is not available. Cannot tell if response is normalized, so transfer function will be used without scaling.\n");
   }
   else if (!Unset(calib) && calib != 0 && calib != 1) {
	  NormStatus = Normalized;
	  printf( "\tResponse appears to be %s.\n", NormStatus == Normalized ? "Normalized" : "Un-Normalized" );
   }
   else{
      NormStatus = IsNormalized( *calper, nfreq, delfrq, xre, xim ) ? Normalized : UnNormalized;
      printf( "\tResponse appears to be %s.\n", NormStatus == Normalized ? "Normalized" : "Un-Normalized" );
   }


   /* Now determine whether scale has been applied... */
   if( Unset(calib) ){
      printf( "\tCalib is not available. Cannot tell if scale factor has been applied on input, so transfer function will be used without scaling.\n");
      ScaleStatus = ScaleUnknown;
   }
   else{
      if( fabs( calib - *scale ) < 0.01 ){
	 ScaleStatus = UnScaled;
	 printf( "\tWaveform appears to be unscaled.\n");
      }
      else{
	 ScaleStatus = Scaled;
	 printf( "\tWaveform appears to be scaled.\n");
      }
   }


   EffectiveCalib = getEffectiveCalib( calib, ncalib, calratio );

   if( NormStatus == UnNormalized && ScaleStatus == Scaled ){
      DataMultiplier = 1.0 / EffectiveCalib;
   }
   else if( NormStatus == Normalized && ScaleStatus == UnScaled ){
      DataMultiplier = EffectiveCalib;
   }
   printf( "\tWaveform multiplied by %f after deconvolution.\n", DataMultiplier );


   return DataMultiplier;
}
/* ---------------------------------------------------------------------------------- */

void NormalizeTransferFunction(nfreq, delfreq, xre, xim, calper, sign)
int nfreq;
double delfreq;
double xre[];
double xim[];
double calper;
double sign;
{
	int i,j;
	double* frequencies =  (double *) malloc(nfreq * sizeof(double));
    for (i = 0; i < nfreq; i++) {
        double freq = i * delfreq;
        frequencies[i] = freq;
    }

	double calFreq = 1.0 / calper;
	int idx = -1;
	double minDiff = DBL_MAX;
	for (j = 0; j < nfreq; ++j) {
		double freqDiff = fabs(calFreq - frequencies[j]);
		if (freqDiff < minDiff) {
			idx = j;
			minDiff = freqDiff;
		}
	}
	free(frequencies);

	if (idx < 0) {
		printf("Response normalization failed!");
		return;
	}

	double c = xre[idx];
	double d = xim[idx];
	double denom = sqrt(c * c + d * d) * sign;
	for (j = 0; j < nfreq; j++) {
		xre[j] /= denom;
		xim[j] /= denom;
	}
}


void /*FUNCTION*/ transfer(dat, npts, delta, fpfrom, ipfrom, kpfrom,
	 kpfrom_s, fpto, ipto, kpto, kpto_s, f, iprew, sre, sim, nfft,
	 xre, xim, nfreq, nerr)
float dat[];
int npts;
double delta;
float fpfrom[];
int ipfrom[];
char kpfrom[MAXKP][MCPFN+1];   int kpfrom_s;
float fpto[];
int ipto[];
char kpto[MAXKP][MCPFN+1];   int kpto_s;
float f[];
int *iprew;
double sre[], sim[];
int nfft;
double xre[], xim[];
int nfreq, *nerr;
{
	char errmsg[131];
	int i, j;
	float a[21];
        float nmScale = 1 ;  /* for EVALRESP:  scales meters to nanometers. */
	double delfrq, denr, fac, freq;
        char sta[9];
	char chan[9];
	double magnitude;
	double phase;
	double radToDeg = 180.0 / acos(-1.0);

	float *const F = &f[0] - 1;

	double DataMultiplier = 1.0;
	double calper = 0.0;
	FILE* fid;

	/*=====================================================================
	 * PURPOSE:  To apply an instrument transfer function to a data set.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *=====================================================================
	 * MODULE/LEVEL:  ICM/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871023:  Major restructuring of instrument parameter storage.
	 *    870317:  Now passing in work arrays rather than using local ones.
	 *    870219:  Added prewhiten/dewhiten for flatter spectrum
	 *             and changed order of spectral operations
	 *    861105:  Original version from K. Nakanishi's TRANSFER program.
	 *=======================================================================
	 * DOCUMENTED/REVIEWED:  870317
	 *===================================================================== */
	/* PROCEDURE: */
	delfrq = 1.0e0/((double)( nfft )*(double)( delta ));


	/* - Prewhiten the data if requested. */
	if( *iprew > 0 ){
	    errmsg[ 0 ] = '\0' ;
	    prewit( dat, npts, iprew, a, NULL , errmsg );
	    if( errmsg[ 0 ] )
		fprintf( stdout, "%s \n", errmsg );
	}

	/* - Deconvolve seismometer. */
        setTransferDirection(FROM);
	dseis( nfreq, delfrq, xre, xim, fpfrom, ipfrom, kpfrom,kpfrom_s, &nmScale, nerr );
	if( *nerr != 0 )
	    goto L_8888;

        strcpy(sta, kstnm);
	strcpy( chan, kcmpnm );
	deblank(sta);
	deblank(chan);
	printf( "\nStation (%s), Channel (%s)\n", sta, chan );
        /* Attempt to determine whether data have been scaled or not and if transfer
	   function is normalized or not. Based on that information, the data may need
	   to be scaled. This function must be called immediately after getting the
	   deconvolution transfer function before the real and imaginary arrays have been
	   modified.
	*/
        DataMultiplier = GetNormalizationFactor( nfreq, delfrq, xre, xim, &calper );
        /* compute 1 / Transfer function applying waterlevel of DBL_MIN */

	if (DataMultiplier != 1) {
		double sgn = 0.0;
		if (DataMultiplier != 0.0) {
			if (sign(1.0, DataMultiplier)) {
				sgn = 1.0;
			} else {
				sgn = -1.0;
			}
		}
		NormalizeTransferFunction(nfreq, delfrq, xre, xim, calper, sgn);
	}

	for( i = 0; i < nfreq; ++i ){
	    denr=(double) powi(xre[i], 2) + (double) powi(xim[i],2);
	    if(denr <= DBL_MIN){
	      sre[i]=0.0;
	      sim[i]=0.0;
	    }
	    else{
	      denr = 1.0e0/denr;
	      sre[i] = xre[i]*denr;
	      sim[i] = -xim[i]*denr;
	    }
	}

	/* - Determine seismometer transfer function in the 'TO' direction. */
        setTransferDirection(TO);
	dseis( nfreq, delfrq, xre, xim, fpto, ipto, kpto,kpto_s, &nmScale, nerr );

	if( *nerr != 0 )
	    goto L_8888;

        /* Multiply the two transfer functions together to get a composite transfer function... */
	for( i = 0; i < nfreq; i++ ){
	    double temp = xre[i]*sre[i] - xim[i]*sim[i];
	    xim[i] = xre[i]*sim[i] + xim[i]*sre[i];
	    xre[i] = temp;
	}

        /* Apply the taper to the composite transfer function... */
	for( i = 0; i < nfreq; i++ ){
	    freq = (double)( i )*delfrq;
	    fac = delfrq*taper( freq, (double)( F[2] ), (double)( F[1] ) )*
	     taper( freq, (double)( F[3] ), (double)( F[4] ) );
	    xre[i] *= fac;
	    xim[i] *= fac;
	}

	/* - Fill a complex vector (sre +i*sim) [Note reuse of these arrays] and then transform the data. */
	for( i = 0; i < npts; ++i ){
	    sre[i] = (double)( dat[i] )*(double)( delta );
	    sim[i] = 0.0e0;
	}
	for( i = npts; i < nfft; ++i ){
	    sre[i] = 0.0e0;
	    sim[i] = 0.0e0;
	}

	dcpft( sre, sim, nfft, 1, -1 );


	/* - Multiply transformed data by composite transfer operator. */
	for( i = 0; i < nfreq; ++i ){
	    double tempR = xre[i]*sre[i] - xim[i]*sim[i];
	    double tempI = xre[i]*sim[i] + xim[i]*sre[i];
	    sre[i] = tempR;
	    sim[i] = tempI;
	    /* Input data are real so F(N-j) = (F(j))*   */
	    if (i > 0 && i < nfreq-1){
	      j = nfft - i;
	      sre[j] = tempR;
	      sim[j] = -tempI;
	    }
	}


	/* - Perform the inverse transform. */

	dcpft( sre, sim, nfft, 1, 1 );

	/* - Copy the transformed data back into the original data array. */

	for( i = 0; i < npts; ++i )                     /* nmScale is 1 by default, but if */
	    dat[i] = sre[i] * nmScale * DataMultiplier; /* EVALRESP is used, nmScale converts to nm */
	                                                /* DataMultiplier is used to apply or unapply calib */

	/* - Undo the effects of prewhitening if necessary. */

	if( *iprew > 0 ){
	    errmsg[ 0 ] = '\0' ;
	    dewit( dat, npts, *iprew, a, errmsg );
	    if( errmsg[ 0 ] )
		fprintf( stdout, "%s \n", errmsg );
	}


L_8888:
	return;

} /* end of function */

