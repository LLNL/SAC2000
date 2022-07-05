#include <stdio.h>
#include <stdlib.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

void subtract(input1,input2,output,npoints,nerr)
float *input1, *input2, *output;
int npoints;
int *nerr;

{
  int i;

  *nerr = 0;

  for(i=0; i<npoints; i++){
    *(output++) = *(input1++) - *(input2++);
  }

L_8888:
  return;
  
  }


