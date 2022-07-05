#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
void /*FUNCTION*/ map_hdr_out(memarray,buffer, writehdr)
float *memarray, *buffer;
int *writehdr;
{
/*
  This routine maps the in-memory SAC header to the file
  header format.  See map_header_in for details of the
  format differences.
*/
  char *ptr1, *ptr2;
  int i;
/* First get the header values for the non character   */
/* fields fhdr, nhdr, ihdr and lhdr.  These are copied */
/* straight across.                                    */

  ptr1 = (char *)memarray;
  ptr2 = (char *)buffer;
  
 
    /*************************/
 /* ganz begin*/
  char *ptr4, *ptr5;
  ptr4 = (char *)memarray;
  ptr5 = (char *)memarray;
  
  int idx;
  int lswap = 0;
/* ganz end */ 
  
  /* only need to swap if we are writing the header and we swapped bytes 
     during reading of the header
  */
 if (*writehdr==1) {
     
       /* ganz: byteswap header data if necessary. */
      for( idx = 0 ; idx <= 65 ; idx++, ptr4 += 4 ){
            /*printf("DEBUG: idx = %i  prt4 = %f \n",idx, *((float *) ptr4));*/
            if (idx==64){
                /*printf("Header line 64 = %f \n",(*((float *) ptr4)));*/
                if (*((float *) ptr4) == -1.0){
                    lswap = 1;
                    *((float *) ptr4) = -12345.0; /* reset */
                }
            }
      }
      
      if( lswap ){
        /*printf("DEBUG: header must be swapped \n");*/
        for( idx = 0 ; idx < MCMHDR ; idx++, ptr5 += 4 ){
          byteswap( (void *)ptr5, 4 ) ;     
        } 
      
        /*printf("DEBUG: Done with swap \n");*/ 
        *writehdr=-1;
      }
       /* ganz: end if( lswap ) */
 }
   /*************************/                
        
  memcpy(ptr2,ptr1,MCMHDR*sizeof(float));
  
/* Now copy the character variables into the output    */
/* buffer, eliminating the null termination            */
/* character.                                          */

  map_chdr_out(memarray+MCMHDR,buffer+MCMHDR);

 
 
  
  
  
  return;

}
