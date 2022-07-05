#include <stdio.h>
#include <math.h>

#include "coda.h"

void calc_moment_magnitude(struct envelope *envelopes,int nbands,struct g_params *global_params)
{
  int i, Moment_count=0, Mb_count=0;
  float Moment, Mw, freq, Mb;
  char min[10], max[10];
  char status;

  Moment = 0.0;
  Moment_count = 0;
  Mb = 0.;
  Mb_count = 0;

  for(i=1;i<=nbands;i++) {
    sprintf(envelopes[i].ResidStat," ");
    sprintf(envelopes[i].SNRStat," ");
    envelopes[i].Moment = 0.0;
    //if data has been picked, assume it is good!
    if((envelopes[i].fit_window_picked == WINDOW_FROM_PICKFILE) || 
       (envelopes[i].fit_window_picked == WINDOW_FROM_INTERACTIVE)) {
      envelopes[i].Moment = envelopes[i].Dist_Corrected_CodaAmp + envelopes[i].Moment_correction;
      //if automatic fit, check residual and SNR
    } else if((envelopes[i].fit_window_picked != WINDOW_NOPICK) && 
	      (envelopes[i].fit_residual <= envelopes[i].max_residual ) && 
	      (envelopes[i].fit_SNR >= envelopes[i].minimum_SNR)) {
      envelopes[i].Moment = envelopes[i].Dist_Corrected_CodaAmp + envelopes[i].Moment_correction;
    }     

    if( envelopes[i].Moment != 0.0) {
      if (Moment_count <= global_params->Max_Moment_count) {
         Moment = Moment + envelopes[i].Moment;
         Moment_count++;
      }
      /* calculate Mb */
      if(envelopes[i].Mb_weight > 0.0) {
	Mb = Mb + envelopes[i].Mb_weight*envelopes[i].Moment;
	Mb_count++;
      }
    }

    if(envelopes[i].fit_residual > envelopes[i].max_residual) {
      sprintf(envelopes[i].ResidStat,"*");
    }
    if(envelopes[i].fit_SNR < envelopes[i].minimum_SNR) {
      sprintf(envelopes[i].SNRStat,"*");
    }

  }
  /* Now combine some of the moments to get magnitude */
  if(Moment_count > 0) {
    global_params->Moment = Moment / Moment_count;
    global_params->Mw = (global_params->Moment * global_params->Mw_slope) - global_params->Mw_intercept;
  } else {
    global_params->Moment = 0.0;
    global_params->Mw = 0.0;
  }
  if(Mb_count > 0) {
    Mb = Mb / Mb_count;
    //    fprintf(stderr,"Mb= %f count= %d con= %f scale= %f \n",Mb,Mb_count,global_params->Mb_intercept, global_params->Mb_slope);
    global_params->Mb = (Mb - global_params->Mb_intercept)/global_params->Mb_slope;

    
  } else {
    global_params->Mb = 0.0;
  }
}

