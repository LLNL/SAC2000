#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
void /*FUNCTION*/ map_hdr_in(memarray,buffer,lswap)
float *memarray, *buffer;
int lswap ;
{
/*
  This routine maps the SAC file header into memory from
  a buffer containing it in disk file format.  The difference
  between the in-memory format and the disk file format
  is in the storage of the string fields (kmhdr.khdr).
  In the disk file (deriving from the FORTRAN version of
  SAC) these strings are essentially concatenated without
  null termination.  In memory, they are carried about as
  null terminated strings.  This routine picks out the 
  strings and null terminates them, storing them in the
  in memory working storage area.
*/
  char *ptr1, *ptr2, *ptr3, *ptr4;
  int idx;
/* First get the header values for the non character   */
/* fields fhdr, nhdr, ihdr and lhdr.  These are copied */
/* straight across.                                    */

  /*
  
  Note: 
  we need to save the fact that we did a byte swap in case the user modifies
  the header, it will need to be in the same orientation as the other headers
  when written to disk.  To do this we are using a previously unused field, 
  unused#6 and now calling it swayHeaderByte.  ganz 6/16/11
idx = 53  prt3 = 54.215324      gcarc
idx = 54  prt3 = -12345.000000 
idx = 55  prt3 = -12345.000000  
idx = 56  prt3 = 710880.812500  depmen
idx = 57  prt3 = 0.000000       cmpaz
idx = 58  prt3 = 0.000000       cmpinc
idx = 59  prt3 = -12345.000000  xmin
idx = 60  prt3 = -12345.000000  xmax
idx = 61  prt3 = -12345.000000  ymin
idx = 62  prt3 = -12345.000000  ymax
idx = 63  prt3 = -12345.000000  [unused6]  
idx = 64  prt3 = -12345.000000    NEW! swapHeaderByte, this is a temp flag to 
indicate that we swapped bytes, used when writing headers for
sac files brought over from a machine arch. in which bytes are different endian.
  

  */
  ptr1 = (char *)memarray;
  ptr2 = (char *)buffer;
  ptr3 = (char *)memarray;
  
  memcpy(ptr1,ptr2,MCMHDR*sizeof(float));
  

  
  /* byteswap numeric data if necessary. */
  if( lswap ){
    for( idx = 0 ; idx < MCMHDR ; idx++, ptr1 += 4 ){
      byteswap( (void *)ptr1, 4 ) ;     
    }
    /* set the 64th. item to -1 which means swap header, to act as a temp flag ganz */
   
   for( idx = 0 ; idx <= 65 ; idx++, ptr3 += 4 ){
        if (idx==64)
            *((float *) ptr3) = -1.0;
  }
    
    
  } /* end if( lswap ) */
  
  
  

/* Now copy the character variables into the memory    */
/* buffer, supplying the additional null termination   */
/* character.                                          */


  
  map_chdr_in(memarray+MCMHDR,buffer+MCMHDR);
/*
    for( idx = 0 ; idx < 70 ; idx++, ptr4 += 4 ){
       
        printf("DEBUG: idx = %i  prt4 = %f \n",idx, *((float *) ptr4));
  }
*/
  return;

}
