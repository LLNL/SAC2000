#include <stdio.h>
#include <stdlib.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

/* output_image is assumed to be width*height_out int */

void adjust_height(input_image, width, height, output_image, height_out, 
                  ymin, ymax, nerr)
float *input_image;
unsigned int width, height;
float *output_image;
unsigned int height_out;
float ymin, ymax;
int *nerr;
{
  int i,j;
  float *input_vector, *output_vector;

  if((input_vector = (float *)malloc(height*sizeof(float))) == NULL){
    printf("error allocating input_vector--adjust_height\n");
    *nerr = 301;
    return;
  }

  if((output_vector = (float *)malloc(height_out*sizeof(float))) == NULL){
    printf("error allocating output_vector--adjust_height\n");
    *nerr = 301;
    free(input_vector);
    return;
  }

  /* use the linear interpolation routine to adjust data height */

  for( i=0; i<width; i++){
    for (j=0; j<height; j++)input_vector[j] = input_image[j*width+i];
    linear_interp(input_vector, ymin, ymax, (int)height,
                  output_vector, (int)height_out, nerr);
    for (j=0; j<height_out; j++)output_image[j*width+i] = output_vector[j];
  } 

  free(input_vector);
  free(output_vector);

  return;
}
