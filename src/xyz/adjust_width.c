#include <stdio.h>
#include <stdlib.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

/* output_image is assumed to be width_out*height int */

void adjust_width(input_image, width, height, output_image, width_out, 
                  xmin, xmax, nerr)
float *input_image;
unsigned int width, height;
float *output_image;
unsigned int width_out;
float xmin, xmax;
int *nerr;
{
  int i;

  /* use the linear interpolation routine to adjust data width */

  for( i=0; i<height; i++){
    linear_interp(&input_image[i*width], xmin, xmax, (int)width,
                &output_image[i*width_out], (int)width_out, nerr);
  } 

  return;
}
