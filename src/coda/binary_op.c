#include <math.h>
#include <stdio.h>
#include <string.h>

int binary_op (float* data1, float* data2, int n, char* op)
{
  int i,j;

  if (!(strcmp(op,"addf"))) {
    for(i=0;i<n;i++) {
       *(data1+i) = *(data1+i) + *(data2+i);
    }
  }
  else if (!(strcmp(op,"copy"))) {
    for(i=0;i<n;i++) {
       *(data2+i) = *(data1+i);
    }
  }
  else {
    fprintf(stderr, "ERROR! Unknown Operation: %s \n",op);
    return(-1);
  }
  return(0);
}
