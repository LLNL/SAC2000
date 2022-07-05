#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
void /*FUNCTION*/ map_chdr_in(memarray,buffer)
float *memarray, *buffer;
{

/* Copy the character header variables into the memory */
/* buffer, supplying the additional null termination   */
/* character.                                          */


  char *ptr1, *ptr2;
  int i;

  ptr1 = (char *)memarray;
  ptr2 = (char *)buffer;

  memcpy(ptr1,ptr2,8); *(ptr1+8) = '\0';
  ptr1 += 9;  ptr2 += 8;

  memcpy(ptr1,ptr2,16);*(ptr1+16) = '\0';
  ptr1 += 18; ptr2 += 16;

  for (i=0; i<21; i++){
    memcpy(ptr1,ptr2,8);
    *(ptr1+8) = '\0';
    ptr1 += 9;
    ptr2 += 8;
  }

  return;

}
