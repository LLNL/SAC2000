#define NPMAX 6
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "coda.h"

int current_band;
float ampresidp();
struct envelope *envelopes;
float CF_begin;
float pdist;
void calc_codaGF(struct envelope *env, float dist); 

void amoebap(p,y,mp,np,ftol)
     float p[NPMAX+1][NPMAX],y[NPMAX];
     int mp,np;
     float ftol;
{
#define  ITMAX 1000
  float alpha=1.0,beta=0.5,gamma=2.0;
  float pr[NPMAX],prr[NPMAX],pbar[NPMAX];
  int iter,ilo,ihi,inhi,i,j,m,n;
  float ypr,yprr,rtol;
 
  for (iter=0;iter<ITMAX;iter++) {
    ilo=0;
      if(y[0] > y[1]) {
        ihi=0;
        inhi=1;
      }
      else {
        ihi=1;
        inhi=0;
      }
      for( i=0;i<mp;i++) {
        if(y[i] < y[ilo]) 
	  ilo=i;
        if(y[i] > y[ihi]) {
          inhi=ihi;
          ihi=i;
	}
        else 
	  if(y[i] > y[inhi]) 
	    if(i != ihi) 
	      inhi=i;
      }
      rtol=2.*fabs(y[ihi]-y[ilo])/(fabs(y[ihi])+fabs(y[ilo]));
      if(rtol < ftol)
	return;
      for (j=0;j<np;j++)
        pbar[j]=0.;
      
      for (i=0;i<mp;i++) {
        if(i != ihi)
          for(j=0;j<np;j++)
            pbar[j]=pbar[j]+p[i][j];
      }
      for (j=0;j<np;j++) {
        pbar[j]=pbar[j]/np;
        pr[j]=(1.+alpha)*pbar[j]-alpha*p[ihi][j];
      }
      ypr=ampresidp(pr);
      if(ypr <= y[ilo]) {
	for( j=0;j<np;j++)
	  prr[j]=gamma*pr[j]+(1.-gamma)*pbar[j];
	yprr=ampresidp(prr);
	if(yprr < y[ilo]) {
	  for( j=0;j<np;j++)
	    p[ihi][j]=prr[j];
	  y[ihi]=yprr;
	}
	
	else {
	  for( j=0;j<np;j++)
	    p[ihi][j]=pr[j];
	  y[ihi]=ypr;
	}
      }
      else if (ypr >= y[inhi]) {
	if(ypr < y[ihi]) {
	  for(j=0;j<np;j++)
	    p[ihi][j]=pr[j];
	  y[ihi]=ypr;
	}
	for(j=0;j<np;j++)
	  prr[j]=beta*p[ihi][j]+(1.-beta)*pbar[j];
	yprr=ampresidp(prr);
	if(yprr < y[ihi]) {
	  for(j=0;j<np;j++)
	    p[ihi][j]=prr[j];
	  y[ihi]=yprr; 
	}
	else {
	  for(i=0;i<mp;i++) {
	    if(i!=ilo) {
	      for(j=0;j<np;j++) {
		pr[j]=0.5*(p[i][j]+p[ilo][j]);
		  p[i][j]=pr[j];
	      }
	      y[i]=ampresidp(pr);
	    }
	  }
	}
      }
      else {
	for(j=0;j<np;j++)
	  p[ihi][j]=pr[j];
	y[ihi]=ypr;
      }
      
    }
}




void fit_coda_params(struct envelope *Passed_envelopes,float abegin,float dist)
{
  float xx[NPMAX];

  int mp,np,ndim,n,m,i,j;
  float pzero[NPMAX],lam[NPMAX],p[NPMAX+1][NPMAX],yy[NPMAX+1];
  float ftol,srvar,niter;

  envelopes = Passed_envelopes;
  CF_begin = abegin;
  pdist = dist;

  /*** note that the precision of the result depend on the choice 
       c of lam(i).   I think small lambda's are better but slower.  */
  
  /*parameter2 (1) is number of points in fit
    parameter1 is amplitude scale factor (coda amplitude)
    parameter 3 is gamma
    parameter 4 is b0 */
  /*       initial guesses at parameters*/
    
  pzero[0] = 5.0; /* need to fix */
  pzero[1] = FIT_INITIAL_LENGTH; 
  pzero[2] = 0.5;
  pzero[3] = 0.0;

  /*     uncertainties in parameters
	 to fix a parameter, set lam = 0 */
  lam[0] = .02;
  lam[1] = 100;
  lam[2] = 0.1;
  lam[3] = .0001;

  np = 4;

  /*  initialize parameter arrays*/
  mp = np + 1;
  for(m=0;m<mp;m++){
    for(n=0;n<np;n++){
      if (n != m) 
	p[m][n] = pzero[n];
      else 
	p[m][n] = pzero[n] + lam[n];
      xx[n] = p[m][n];
    }
    yy[m] = ampresidp (xx);
  }

  /*  use the simplex algorithm to find the best-fitting parameters */
  ftol = 0.0001;
  amoebap (p,yy,mp,np,ftol);
    
  /*   need to be here if fit is done and results are to be summarized */
    
  for(i=0;i<np;i++)
    xx[i] = 0.0;
  
  for(m=0;m<mp;m++)
    for(n=0;n<np;n++)
      xx[n] = xx[n] + p[m][n] / (float) mp;
    
  srvar = ampresidp(xx);
  /*      fprintf(stdout, "Residual: %f \n", srvar);
          fprintf(stdout, "Number of points:  %f \n", xx[1]);
          fprintf(stdout, "Coda Amplitude: %f \n", xx[0]);
  */
  envelopes->CodaAmp =  xx[0];
  envelopes->fit_residual = srvar;
  envelopes->fit_npoints = (int) xx[1];
  envelopes->fit_b = xx[2];
  envelopes->fit_gamma = xx[3];
}



float ampresidp(xx)
     float xx[NPMAX];
{
  
  int i,j,window_start,npoints,min_length;
  float resid=0.0, amp, diff, b0, gamma;
  /* xx[1] is npoints, xx[0] is amplitude */
  min_length = (int) (envelopes->min_length / envelopes->GFdelta);
  window_start = envelopes->window_start;
  npoints = (int) xx[1];
  amp = xx[0];
  b0 = xx[1];
  gamma = xx[2];
  calc_codaGF(envelopes,pdist);
  if( envelopes->fit_window_picked != WINDOW_FROM_FIT) { /* fix npoints */
    npoints = envelopes->fit_npoints;
  }
  j = window_start-CF_begin;
  if((npoints < min_length) || npoints >= envelopes->number_of_points || npoints >= NPTS_GF) return(10000);
  for(i=0;i<npoints;i++) {
    diff = fabs(*(envelopes->GFenvelope+i)+amp - *(envelopes->envelope_data+j));
    resid = resid + diff*(FIT_FUNC_MULT*(i-FIT_FUNC_SUB)*(i-FIT_FUNC_SUB) + 1);
    j++;
  }
    return resid/npoints;
}


