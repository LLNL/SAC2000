#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "coda.h"

FILE	*fpcfg;		 /* Pointer to the configuration input file  */

int get_input( char *inputfile,struct envelope *envelopes,int *nbands,struct g_params *global_params)
{
  int     i,j,k,ifreq,length;
  int  tempint, nchar=400;
  float tempfloat, tempfloat1;
  char tempchar[100],charbuf[400],readbuf[400];
  char *rp, *key, *name, *tempc;
  struct envelope *env;

  key = &charbuf[0];

  length = strcspn(inputfile," ");
  inputfile[length] = '\0';

  if((fpcfg=fopen(inputfile,"r"))== NULL) {
    perror("Error opening input file \n");
    fprintf(stderr,"\n\t%%Error opening input file %s; Exiting.\n",inputfile);
    return -1;
  }
  /*
  global_params->Mb_slope = MB_SLOPE;
  global_params->Mb_intercept = MB_INTERCEPT;
  global_params->Mw_slope = MW_SLOPE;
  global_params->Mw_intercept = MW_INTERCEPT;
  global_params->Max_Moment_count = MAX_MOMENT_COUNT;

  global_params->Max_Residual = MAX_RESIDUAL;
  global_params->Minimum_SNR = MINIMUM_SNR;
  global_params->rho = RHO;
  global_params->Beta = BETA;
  global_params->mu = MU;
  */
  //  fprintf(stderr,"rho= %f beta= %f mu= %f \n",global_params->rho, global_params->Beta, global_params->mu);
  /* need to fix bug if extra CRLF at end of file */
  ifreq = 0;
  while(fgets(charbuf, nchar, fpcfg)!=NULL) {
    //  fprintf(stderr,"i= %d rho= %f beta= %f mu= %f \n",ifreq,global_params->rho, global_params->Beta, global_params->mu);
    strcpy ( readbuf , charbuf ) ;
    rp = &readbuf[0];
    key = &tempchar[0];
    sscanf(rp,"%s",key);
    rp = strtok(rp," ");
    // global parameters 
    if (!(strcmp(key,"mb_slope")) || !(strcmp(key,"MB_SLOPE")) || !(strcmp(key, "Mb_Slope")) || !(strcmp(key, "Mb_slope"))) {
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      global_params->Mb_slope = tempfloat;
    } else if
      (!(strcmp(key,"mb_intercept")) || !(strcmp(key,"MB_INTERCEPT")) || !(strcmp(key, "Mb_Intercept")) || !(strcmp(key, "Mb_intercept"))) {
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      global_params->Mb_intercept = tempfloat;
    } else if
      (!(strcmp(key,"mw_slope")) || !(strcmp(key,"MW_SLOPE")) || !(strcmp(key, "Mw_Slope")) || !(strcmp(key, "Mw_slope"))) {
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      global_params->Mw_slope = tempfloat;
    } else if
      (!(strcmp(key,"mw_intercept")) || !(strcmp(key,"MW_INTERCEPT")) || !(strcmp(key, "Mw_Intercept")) || !(strcmp(key, "Mw_intercept"))) {
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      global_params->Mw_intercept = tempfloat;
    } else if
      (!(strcmp(key,"max_moment_count")) || !(strcmp(key,"MAX_MOMENT_COUNT")) || !(strcmp(key, "Max_Moment_Count")) || !(strcmp(key, "Max_Moment_count"))) {
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      global_params->Max_Moment_count = tempfloat;
    } else if
      (!(strcmp(key,"Max_residual")) || !(strcmp(key,"MAX_RESIDUAL")) || !(strcmp(key, "Max_Residual")) || !(strcmp(key, "max_residual"))) {
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      global_params->Max_Residual = tempfloat;
    } else if
      (!(strcmp(key,"Minimum_Snr")) || !(strcmp(key,"MINIMUM_SNR")) || !(strcmp(key, "Minimum_SNR")) || !(strcmp(key, "minimum_snr"))) {
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      global_params->Minimum_SNR = tempfloat;
    } else if
      (!(strcmp(key,"rho")) || !(strcmp(key,"RHO")) || !(strcmp(key, "Rho"))) {
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      global_params->rho = tempfloat;
    } else if
      (!(strcmp(key,"mu")) || !(strcmp(key,"MU")) || !(strcmp(key, "Mu"))) {
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      global_params->mu = tempfloat;
    } else if
      (!(strcmp(key,"beta")) || !(strcmp(key,"BETA")) || !(strcmp(key, "Beta"))) {
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      global_params->Beta = tempfloat;
    } else if
      ((strncmp(key,"#",1)) && (strncmp(key, "*",1)) && (strncmp(key, "//",1))) {
      ifreq++;
      //      env = envelopes+ifreq;
      sprintf(envelopes[ifreq].ResidStat," ");
      sprintf(envelopes[ifreq].SNRStat," ");
      strcpy ( readbuf , charbuf ) ;
      rp = strtok(readbuf," ");

      //frequency
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].freq_low = tempfloat;
      //            env->freq_low = tempfloat;
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].freq_high = tempfloat;
      //            env->freq_high = tempfloat;
      
      //b0
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].b0 = tempfloat;

      //b1
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].b1 = tempfloat;

      //b2
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].b2 = tempfloat;

      //gamma0
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].gamma0 = tempfloat;

      //gamma1
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].gamma1 = tempfloat;

      //gamma2
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].gamma2 = tempfloat;

      //vel0
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].vel0 = tempfloat;

      //vel1
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].vel1 = tempfloat;

      //vel2
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].vel2 = tempfloat;

      //p1
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].path_p1 = tempfloat;

      //p2
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].path_p2 = tempfloat;

      //moment correction
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].Moment_correction = tempfloat;

      //minimum_SNR
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].minimum_SNR = tempfloat;

      //max_residual
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].max_residual = tempfloat;

      //minimum_length
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].min_length = tempfloat;

      //maximum_length
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].max_length = tempfloat;

      //Mb_weight
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].Mb_weight = tempfloat;

      //normalization time
      rp = strtok(NULL," ");
      sscanf(rp,"%f",&tempfloat);
      envelopes[ifreq].tnorm = tempfloat;
    }
  }
  *nbands = ifreq;
  fclose(fpcfg);

  //  fprintf(stderr,"rho= %f beta= %f mu= %f \n",global_params->rho, global_params->Beta, global_params->mu);
  for(i=1;i<=*nbands;i++) {
      envelopes[i].fit_window_picked = WINDOW_FROM_FIT;
      //flag all bands as default window
  /*
            fprintf(stderr,"%d %4f %4f %4f %4f %4f %4f %4f %4f %4f %4f %4f %4f %4f %4f %4f %4f %4f %4f %4f \n", \
	      i,
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
  */	      
  }

  return 0;
}

int get_picks(char *inputfile,struct envelope *envelopes, int nbands)
{
  int     i,j,k,ifreq,length;
  int  tempint, nchar=400;
  float tempfloat;
  float freql, freqh, pfreql, pfreqh;
  char tempchar[100],charbuf[400],readbuf[400];
  char *rp, *key, *name, *tempc;

  key = &charbuf[0];

  //flag all bands as unpicked
  for(i=1;i<=nbands;i++) {
    envelopes[i].fit_window_picked = WINDOW_NOPICK;
  }

  length = strcspn(inputfile," ");
  inputfile[length] = '\0';

  if((fpcfg=fopen(inputfile,"r"))== NULL) {
    perror("Error opening input file \n");
    fprintf(stderr,"\n\t%%Error opening input file %s; Exiting.\n",inputfile);
    return -1;
  }

  /* need to fix bug if extra CRLF at end of file */
  while(fgets(charbuf, nchar, fpcfg)!=NULL) {
    strcpy ( readbuf , charbuf ) ;
    rp = &readbuf[0];
    key = &tempchar[0];
    sscanf(rp,"%s",key);
    //allow for comment lines
    if ((strncmp(key,"#",1)) && (strncmp(key, "*",1)) && (strncmp(key, "//",1))) {
      strcpy ( readbuf , charbuf ) ;
      rp = strtok(readbuf," _");

      //frequency
      sscanf(rp,"%f",&freql);
      rp = strtok(NULL," _");
      sscanf(rp,"%f",&freqh);

      ifreq = -1;
      for(i=1;i<=nbands;i++) {
	//	fprintf(stderr,"pickfile: %f %f paramfile: %f %f \n",freql,freqh,envelopes[i].freq_low,envelopes[i].freq_high);
	if((freql == envelopes[i].freq_low) && (freqh == envelopes[i].freq_high)) {
	  //	  fprintf(stderr,"got it! %d %f %f\n",i,freql,freqh);
	  ifreq = i;
	  break;
	}
      }
      //      fprintf(stderr,"ifreq= %d nbands= %d \n",ifreq,nbands);
      if( ifreq >= 1) {
	rp = strtok(NULL," _");
	sscanf(rp,"%f",&tempfloat);
	envelopes[ifreq].picked_window = tempfloat;
	if(tempfloat > 0.0) {
	  envelopes[ifreq].fit_window_picked = WINDOW_FROM_PICKFILE;
	} else {
	  envelopes[ifreq].fit_window_picked = WINDOW_NOPICK;
	}
	//arrival adjustment picked_window_start
	rp = strtok(NULL," ");
	sscanf(rp,"%f",&tempfloat);
	envelopes[ifreq].picked_window_start = tempfloat;
      } else {
	// not a valid frequency
	fprintf(stderr, "Invalid frequency band in pickfile: %f %f \n",freql,freqh);
      }
    }
  }
  fclose(fpcfg);
  return 0;
}
