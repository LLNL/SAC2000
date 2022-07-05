#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
void /*FUNCTION*/ map_chdr_out(memarray,buffer)
float *memarray, *buffer;
{

/* Copy the character header variables into the output */
/* buffer, eliminating the null termination character  */
/* (packing the strings one after another)             */

  char *ptr1, *ptr2;
  int i;

  ptr1 = (char *)memarray;
  ptr2 = (char *)buffer;

                   
  memcpy(ptr2,ptr1,8);
  ptr1 += 9;  ptr2 += 8;

  memcpy(ptr2,ptr1,16);
  ptr1 += 18; ptr2 += 16;

  for (i=0; i<21; i++){
    memcpy(ptr2,ptr1,8);
    ptr1 += 9;
    ptr2 += 8;
  }
  
  
  return;

}
