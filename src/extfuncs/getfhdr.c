#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "../../inc/extfunc.h"

void getfield(char* fieldname, int lenfield, char* fieldout);

float getfhdr( header_in, fieldname, error )
sac_header *header_in;
char      *fieldname;
int          *error;

{
  int i;

  *error = 0;

  for( i=0; i<MFHDR; i++){
    if(!strcmp(fieldname,float_hdr_fields[i]))break;
  }

  if( i<MFHDR ) return(header_in->ext_fhdr[i]);
  else {
    *error = 1;
    return (FUNDEF);
  }

}
