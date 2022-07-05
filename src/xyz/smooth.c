#include <stdio.h>
#include <stdlib.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

void smooth(input,npoints,lmean,index,nhalf,nfull,factor,output,nerr)
float *input;
int npoints,lmean;
int *index;
int nhalf,nfull;
float factor;
float *output;
int *nerr;
/* Taken from the xsmooth command */

{

  int j;
  float sum = 0.0;
  float *itemp;
  float *t1, *t2;

  int memerr;

  int malloc_verify();

  *nerr = 0;

  if(lmean){ /* Mean smoothing */
    itemp = input;
    for(j=0; j<nfull; j++)sum += *(itemp++);

    t1 = output + nhalf;
    *(t1++) = factor * sum;
   
    t2 = input + nhalf + 1;

    for(j=nhalf+1; j<=(npoints-1-nhalf); j++){
      sum += (*(t2+nhalf)-*(t2-nhalf-1));
      *(t1++) = factor*sum;
      t2++;

#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

    }
  }
  else{   /* Median smoothing */
    t1 = output + nhalf;
    t2 = input  + nhalf;
    for(j=nhalf; j<=(npoints-1-nhalf); j++){
      srtndx(t2-nhalf,nfull,index,nerr);
      if(*nerr != 0){
        printf("Error returned from srtndx--smooth\n");
        goto L_8888;
      }
      *(t1++) = *(t2-nhalf+index[nhalf]-1);
      t2++;
    }

  }

  /* Replicate end points */
  t1 = output + nhalf - 1;
  for(j=nhalf-1; j>=0; j--)
     *(t1--) = *(output + nhalf);

#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

  t1 = output + npoints - nhalf;
  for(j=(npoints-nhalf); j <= npoints-1; j++){
     *(t1++) = *(output + npoints - 1 - nhalf);
  }

#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

L_8888:
  return;
  
  }


