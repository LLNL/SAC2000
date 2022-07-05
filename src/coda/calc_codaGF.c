#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "coda.h"

void calc_codaGF(struct envelope *env, float dist) 
{
  float time, b, CodaAmp, *GFenvptr;
  float  gamma, e, nt, norm;
  int i, npoints;
  float travel_time, velocity, Ao;
  
  e = exp(1);
  Ao = 1.0;

  velocity = env->vel0 - (env->vel1/(env->vel2 + dist));
  travel_time = dist / velocity;
  time=env->window_start_seconds - travel_time + env->GFstart;
  if( time < 0 ) time = 0;
  npoints = env->GFnpts;
  gamma = env->gamma0 - (env->gamma1/(env->gamma2 + dist));
  b = env->b0 - (env->b1/(env->b2 + dist));
  env->GFenvelope = (float *) malloc(npoints*sizeof(float));
  GFenvptr = env->GFenvelope;
  nt = env->tnorm - travel_time + env->GFstart;
  norm=  Ao - gamma*log10(nt) + b*nt;
  //  fprintf(stderr, "freq=%.2f-%.2f gamma=%f b=%f dist=%f time=%f norm=%f nt=%f\n", env->freq_low,env->freq_high,gamma,b,dist,time,norm,nt); 
  for(i=(int)time;i<npoints;i++) {
    time= time + env->GFdelta;
    CodaAmp = Ao - (gamma*log10(time)) + (b*time) - norm;
    *GFenvptr++ = CodaAmp;

  }

}

