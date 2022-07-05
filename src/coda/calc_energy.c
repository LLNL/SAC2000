#include <stdio.h>
#include <math.h>

#include "coda.h"

void calc_energy(struct envelope *envelopes,int nbands,char* evid,int lcalibrate,struct g_params *global_params)
{
  int i, jj, ibeg, iend;
  float freq, d, df, mu, stress_total, energy, d1, d2, k, w_start, w_end, Amp;
  float f[MAXBANDS], A[MAXBANDS], E[MAXBANDS];
  float mo, Etotal_obs, Etotal_HI, Etotal_low, sum, temp, pi;
  float wN, deltawN, AN, EN, wF, Ahi;

  /*Now compute the total energy by summing the squared product of the 
    discrete path-corrected moment rate spectra (ie. (10**A(1))/d ) 
    Then finally multiply by dw (delta omega) at the front.
  */
  pi = 3.141592654;
  k = sqrt(1.398e-30);
  sum=0.0;
  Etotal_obs=0.0;
  Etotal_HI=0.0;
  Etotal_low=0.0;
  mo=global_params->Moment;
  //decide range of frequency bands to use
  ibeg = -1;
  iend = -1;
  jj = 1;
  while((ibeg < 0) && (jj < nbands)) {
    if((envelopes[jj].Moment != 0.0) && (envelopes[jj+1].Moment) && (envelopes[jj+2].Moment != 0.0)) {
      ibeg=jj;
    } else {
      jj++;
    }
  }

  jj = nbands;
  while((iend < 0) && (jj > 1)) {
    if((envelopes[jj].Moment != 0.0) && (envelopes[jj-1].Moment != 0.0)) {
      iend=jj;
    } else {
      jj--;
    }
  }

  /* Calculate observed energy */
  /* first see if there are enough measurements? */
  for(jj=ibeg;jj<=iend-1;jj++) {
    f[jj] = (envelopes[jj].freq_low + envelopes[jj].freq_high) / 2;
    df = (envelopes[jj].freq_high - envelopes[jj].freq_low);
    A[jj] = pow(10.,envelopes[jj].Moment)*k;
    w_start = 2.0*pi*(envelopes[jj].freq_low);
    w_end = 2.0*pi*(envelopes[jj].freq_high);
    E[jj]= pow(A[jj],2.0)/3.0 * (pow(w_end,3.0) - pow(w_start,3.0)); /* Energy at f(jj) */
    sum = sum + E[jj];
    //    fprintf(stderr," %.3f %.4f %.3f \n",f[jj], envelopes[jj].Moment, df);
  }
  f[iend] = (envelopes[iend].freq_low + envelopes[iend].freq_high) / 2;
  df = envelopes[iend].freq_high - envelopes[iend].freq_low;
  //  fprintf(stderr," %.3f %.4f %.3f \n",f[iend], envelopes[iend].Moment, df);
  wN=2.0*pi*f[iend];
  deltawN=2.0*pi*df/2.0;
  A[iend] = pow(10.,envelopes[iend].Moment)*k;
  AN= A[iend];
  EN=(pow(AN, 2.0)/3.0)*(pow(wN, 3.0) - pow((wN-deltawN),3.0));
  Etotal_obs=sum+EN;

//extrapolated low frequency energy

  df = envelopes[ibeg].freq_high - envelopes[ibeg].freq_low;
  wF=2.0*pi*(f[ibeg]-(df/2.0));
  //  df = envelopes[2].freq_high - envelopes[2].freq_low;
  //  wF=2.0*pi*(f[2]-(df/2.0));
  Amp= pow(10., mo)*k;
  Etotal_low= pow(Amp,2.0)* pow(wF,3.0)/3.0;
  //  fprintf(stderr,"wF= %f Amp= %f ETL= %f\n",wF,Amp,Etotal_low );
//We do the same thing for the high frequency extrapolation.

  wN= 2.0*pi*f[iend];
  Ahi= A[iend];
  Etotal_HI= pow(Ahi,2.0) * pow(wN,3.0);

  //  fprintf(stderr,"ibeg= %d iend= %d wN= %f Ahi= %f ETH= %f\n",ibeg,iend,wN,Ahi,Etotal_HI );
  if((ibeg > 0) && (iend > 0)) {

  } else {
    fprintf(stderr, "Warning! Unable to calculate low and high frequency energy \n");
  }    

  //convert to Joules
  Etotal_obs = log10(Etotal_obs) - 7.0;
  Etotal_HI = log10(Etotal_HI) - 7.0;
  Etotal_low = log10(Etotal_low) - 7.0;

  /*    TOTAL ENERGY */
/*this is the total energy of low,observed and high */
  energy=log10((pow(10,Etotal_low) + pow(10,Etotal_obs) + pow(10,Etotal_HI)) * 1.07);  



  global_params->Energy_Obs = Etotal_obs;
  global_params->Energy_High = Etotal_HI;
  global_params->Energy_Low = Etotal_low;
  global_params->Energy_Total = energy;
  /* Compute stress drops */
  energy = pow(10.,energy + 7.0);
  stress_total=(2.0*global_params->mu*(energy)/pow(10.,mo))/10.0;
  global_params->Stress_Total = stress_total;
}

