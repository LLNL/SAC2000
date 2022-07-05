#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "coda.h"
void fit_coda_params(struct envelope *Passed_envelopes,float abegin,float dist);
void fit_coda_amp(struct envelope *Passed_envelopes,float abegin);

void calc_codaGF(struct envelope *env, float dist);
int binary_op (float* data1, float* data2, int n, char* op);
int unary_op (float* data, int* npts, char* op, float arg, float* delta);
void setfhv_(char* a, float* b, int* c, int d);
void wsac1_(char* outfile,float* data,int *npts, float* begin, float* delta,int* nerr);

void calc_coda_amplitudes(struct envelope *envelopes,int nbands,float dist,float begin,char* evid,int lcalibrate)
{
  int i,j,noisecount,namelength,nptswrite;
  float start_time, stop_time, sigsum, noisesum;
  char filename[200];
  float *databuf1;
  float delta, velocity, tmpFloat;
  float maxamp;
  int shift_window_start,shift_window_end,maxi;

  for(i=1;i<=nbands;i++) {
    envelopes[i].GFnpts = NPTS_GF;
    envelopes[i].GFstart = START_GF;
    envelopes[i].GFdelta = DELTA_GF;

    /* align GF and observed envelopes--find point in observed that corresponds to first point of GF */
    velocity =  envelopes[i].vel0 - (envelopes[i].vel1 / ( envelopes[i].vel2 + dist));
    start_time = (dist / velocity) + START_WINDOW_ADD;
    start_time = (dist / velocity) + 1.0;

    envelopes[i].window_start = (int) ((start_time) / envelopes[i].GFdelta );
    if(( envelopes[i].fit_window_picked == WINDOW_FROM_DEFAULT) || 
       ( envelopes[i].fit_window_picked == WINDOW_FROM_FIT) ||
       ( envelopes[i].fit_window_picked == WINDOW_NOPICK)) { 
      envelopes[i].fit_npoints = (int) (envelopes[i].min_length / envelopes[i].GFdelta);
      envelopes[i].picked_window_start = 0.0;
    } else {
      envelopes[i].fit_npoints = (int) ((envelopes[i].picked_window - start_time) / envelopes[i].GFdelta);
      if( envelopes[i].picked_window_start > 0.0) {
	envelopes[i].window_start = (int) ( envelopes[i].picked_window_start / envelopes[i].GFdelta );
      }	
    }
    //calculate time adjustment if necessary
    envelopes[i].time_adjustment = 0.0;
    /*
    if(( envelopes[i].fit_window_picked == WINDOW_FROM_DEFAULT) || 
       ( envelopes[i].fit_window_picked == WINDOW_FROM_FIT) ||
       ( envelopes[i].fit_window_picked == WINDOW_NOPICK) ||
       ( envelopes[i].picked_window_start <= 0.0)) { 
      //    set window
      shift_window_start = envelopes[i].window_start - (int) (MAX_TIME_ADJUSTMENT / envelopes[i].GFdelta);
      shift_window_end = envelopes[i].window_start + (int) (MAX_TIME_ADJUSTMENT / envelopes[i].GFdelta);
      //find max amp of observed envelope in shift window around start time
      maxamp = -1e10;
      maxi = envelopes[i].window_start;
      //    fprintf(stderr,"finding maximum in window %d %d \n",shift_window_start, shift_window_end);
      for(j=shift_window_start;j<shift_window_end;j++) {
	if(*(envelopes[i].envelope_data+j) > maxamp) {
	  maxamp = *(envelopes[i].envelope_data+j);
	  maxi = j;
	}
      }
      envelopes[i].time_adjustment = (float)( (maxi - envelopes[i].window_start) * envelopes[i].GFdelta);
      envelopes[i].window_start = maxi;
    } 
    */
    envelopes[i].window_start_seconds =	envelopes[i].window_start * envelopes[i].GFdelta;

    //calculate synthetic envelopes
    calc_codaGF(&envelopes[i],dist);

    
    //    fprintf(stderr,"window start for freq %f = %d \n",envelopes[i].freq_low,envelopes[i].window_start);
    if(lcalibrate) {
      fit_coda_params(&envelopes[i],begin,dist);
    } else {
      fit_coda_amp(&envelopes[i],begin);
    }
 
    //write out coda amplitudes to SAC files
    databuf1 = (float *) malloc(envelopes[i].fit_npoints*sizeof(float));
    sprintf(filename, "%s.GFfit-%2.2f_%2.2f",evid,envelopes[i].freq_low,envelopes[i].freq_high);
    binary_op(envelopes[i].GFenvelope,databuf1,envelopes[i].fit_npoints,"copy");
    unary_op(databuf1,&envelopes[i].fit_npoints,"add",envelopes[i].CodaAmp,&delta);
    setfhv_("USER0", &envelopes[i].fit_residual,&nerr,6);
    tmpFloat = envelopes[i].fit_npoints;
    setfhv_("USER1", &tmpFloat,&nerr,6);
    nptswrite=(envelopes[i].fit_npoints);
    if(envelopes[i].fit_npoints > 0)
      namelength = strcspn(filename," ");
    wsac1_(filename,databuf1,&nptswrite,&envelopes[i].window_start_seconds,&envelopes[i].GFdelta,&nerr);
    
    //do path correction
    envelopes[i].Dist_Corrected_CodaAmp = envelopes[i].CodaAmp - \
         log10((1 / (1 + pow( (dist/envelopes[i].path_p2), envelopes[i].path_p1))));

    /* Measure SNR */
    noisesum = 0.;
    noisecount = 0;
    for(j=((int) (NOISE_START/envelopes[i].GFdelta));j<((int) ((NOISE_START+NOISE_LENGTH)/envelopes[i].GFdelta));j++) {
     noisesum = noisesum + *(envelopes[i].envelope_data+j);
     noisecount++;
    }

    noisesum = noisesum/noisecount;
    
    sigsum = 0.;
    noisecount = 0;
    envelopes[i].window_stop = envelopes[i].window_start+envelopes[i].fit_npoints;
    for(j=envelopes[i].window_start;j<envelopes[i].window_stop;j++) {
     sigsum = sigsum + *(envelopes[i].envelope_data+j);
     noisecount++;
    }

    sigsum = sigsum/noisecount;
    envelopes[i].fit_SNR = sigsum - noisesum;
  }

}
