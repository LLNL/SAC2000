#include <stdio.h>
#include <math.h>

#include "coda.h"

int send_output(struct envelope *envelopes,int nbands,char* evid,int lcalibrate,struct g_params *global_params)
{
  int i, Moment_count=0,npoints,start,end;
  float Moment, Mw, freq;
  char filename[200], specname[200], min[10], max[10],pickstatus[2];
  FILE	*fpcfg, *fpspc;		 /* Pointers to the output files  */

  Moment = 0.0;
  Moment_count = 0;
  sprintf(filename, "%s.out",evid);
  if((fpcfg=fopen(filename,"w"))== NULL) {
    perror("Error opening output file \n");
    fprintf(stderr,"\n\t%%Error opening output file %s; Exiting.\n",filename);
    fclose(fpcfg);
    fclose(fpspc);
    return -1;
  }
  sprintf(specname, "%s.spec",evid);
  if((fpspc=fopen(specname,"w"))== NULL) {
    perror("Error opening output file \n");
    fprintf(stderr,"\n\t%%Error opening output file %s; Exiting.\n",specname);
    fclose(fpcfg);
    fclose(fpspc);
    return -1;
  }
  if(lcalibrate) {
    fprintf(stdout, "Stat   Freq     CodaAmp  DCCodaAmp    Mo      Resid        win     SNR   b0  gamma\n");
    fprintf(fpcfg, " Stat   Freq     CodaAmp  DCCodaAmp    Mo      Resid        win     SNR   b0  gamma\n");
  } else {
    fprintf(stdout, "Stat   Freq     CodaAmp    DCCodaAmp   Mo      Resid        win     SNR   time shift\n");
    fprintf(fpcfg,  "Stat   Freq     CodaAmp    DCCodaAmp   Mo      Resid        win     SNR   time shift\n");
  }

  for(i=1;i<=nbands;i++) {
    sprintf(pickstatus, "U");
    freq = (envelopes[i].freq_low + envelopes[i].freq_high) / 2;
    if(envelopes[i].fit_window_picked == WINDOW_NOPICK) {
       sprintf(pickstatus, "X");
    }
    else if(envelopes[i].fit_window_picked == WINDOW_FROM_INTERACTIVE) {
      sprintf(pickstatus, "P");
    }
    else if(envelopes[i].fit_window_picked == WINDOW_FROM_FIT) {
      sprintf(pickstatus, "A");
    }
    else if(envelopes[i].fit_window_picked == WINDOW_FROM_PICKFILE) {
      sprintf(pickstatus, "F");
    }
    else if(envelopes[i].fit_window_picked == WINDOW_FROM_DEFAULT) {
      sprintf(pickstatus, "D");
    } 
    npoints=(int)(envelopes[i].fit_npoints*envelopes[i].GFdelta);
    start= envelopes[i].window_start;
    end= envelopes[i].window_start + npoints;

    if(lcalibrate) {
      fprintf(stdout, " %s     %2.2f     %2.2f     %2.2f   %02.2f    %1.5f%s    %6d  %5.2f%s  %5.4f  %5.2f\n", \
	      pickstatus,freq,envelopes[i].CodaAmp,envelopes[i].Dist_Corrected_CodaAmp,envelopes[i].Moment,envelopes[i].fit_residual, \
	      envelopes[i].ResidStat,npoints,envelopes[i].fit_SNR,envelopes[i].SNRStat,envelopes[i].fit_b,\
	      envelopes[i].fit_gamma );
   /* fprintf(fpcfg, " %s     %2.2f     %2.2f     %2.2f    %02.2f    %1.5f%s   %5d %5d  %5.2f%s  %5.4f  %5.2f\n" \*/
      fprintf(fpcfg, " %s     %2.2f     %2.2f     %2.2f    %02.2f    %1.5f%s   %5d %5.2f  %s %5.2f  %5.4f  \n" \
	      ,pickstatus,freq,envelopes[i].CodaAmp,envelopes[i].Dist_Corrected_CodaAmp, \
	      envelopes[i].Moment,envelopes[i].fit_residual, envelopes[i].ResidStat, npoints, \
	      envelopes[i].fit_SNR,envelopes[i].SNRStat,envelopes[i].fit_b, envelopes[i].fit_gamma );
    } else { 

      fprintf(stdout, " %s     %2.2f     %2.2f     %2.2f     %02.2f    %1.5f%s  %5d %5d  %5.2f%s  %3.1f\n", \
	      pickstatus,freq,envelopes[i].CodaAmp,envelopes[i].Dist_Corrected_CodaAmp, \
	      envelopes[i].Moment,envelopes[i].fit_residual, envelopes[i].ResidStat, \
	      start,end, envelopes[i].fit_SNR, envelopes[i].SNRStat, \
	      envelopes[i].time_adjustment );
      fprintf(fpcfg, " %s     %2.2f     %2.2f     %2.2f     %02.2f    %1.5f%s  %5d %5d   %5.2f%s  %3.1f\n", \
	      pickstatus,freq,envelopes[i].CodaAmp,envelopes[i].Dist_Corrected_CodaAmp, \
	      envelopes[i].Moment,envelopes[i].fit_residual, envelopes[i].ResidStat, \
	      start,end, envelopes[i].fit_SNR, envelopes[i].SNRStat, \
	      envelopes[i].time_adjustment );
    }
    if(envelopes[i].Moment > 0.0) {
      fprintf(fpspc, " %2.2f     %2.2f \n",freq,envelopes[i].Moment );
    }
  }
  if(global_params->Moment > 0.0) {
    fprintf(stdout, "Moment= %f, Mw= %f \n",global_params->Moment, global_params->Mw);
    fprintf(fpcfg, "Log Moment= %f, Mw= %f \n",global_params->Moment, global_params->Mw);
  } else {
    fprintf(stdout, "Unable to calculate Moment and Mw \n");
    fprintf(fpcfg, "Unable to calculate Moment and Mw \n");
  }
  if(global_params->Mb > 0.0) {
    fprintf(stdout, "Mb= %f \n",global_params->Mb);
    fprintf(fpcfg, "Mb= %f \n",global_params->Mb);
  } else {
    fprintf(stdout, "Unable to calculate Mb \n");
    fprintf(fpcfg, "Unable to calculate Mb \n");
  }
  if(global_params->Energy_Total > 0.0) {
    fprintf(stdout, "Total Energy= %e J\n",global_params->Energy_Total);
    fprintf(fpcfg, "Total Energy= %e J\n",global_params->Energy_Total);
    fprintf(stdout, "Energies: Observed= %e High= %e Low=%e \n",global_params->Energy_Obs,global_params->Energy_High,global_params->Energy_Low);
    fprintf(fpcfg, "Energies: Observed= %e High= %e Low=%e \n",global_params->Energy_Obs,global_params->Energy_High,global_params->Energy_Low);
    fprintf(stdout,"Stress Drop= %e bars \n",global_params->Stress_Total);
    fprintf(fpcfg,"Stress Drop= %e bars \n",global_params->Stress_Total);

    } else {
      fprintf(stdout, "Unable to calculate Energy\n");
      fprintf(fpcfg, "Unable to calculate Energy \n");
    }

    fprintf(stdout, "*-Denotes did not pass acceptance threshold for SNR or residual \n");
    fprintf(fpcfg, "*-Denotes did not pass acceptance threshold for SNR or residual \n");

    fprintf(fpcfg,"Input parameters \n");
    for(i=1;i<=nbands;i++) {

      fprintf(fpcfg,"%.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f\n", \
	      envelopes[i].freq_low,
	      envelopes[i].freq_high,
	      envelopes[i].b0,
	      envelopes[i].b1,
	      envelopes[i].b2,	      
	      envelopes[i].gamma0,
	      envelopes[i].gamma1,
	      envelopes[i].gamma2,
	      envelopes[i].vel0,
	      envelopes[i].vel1,
	      envelopes[i].vel2,	      
	      envelopes[i].path_p1,
	      envelopes[i].path_p2,
	      envelopes[i].Moment_correction,
	      envelopes[i].minimum_SNR,
	      envelopes[i].max_residual,
	      envelopes[i].min_length,
	      envelopes[i].max_length,
	      envelopes[i].Mb_weight,
	      envelopes[i].tnorm);
  }

    fclose(fpcfg);
    fclose(fpspc);
    return 0;
}

