#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "../../inc/extfunc.h"
void getfield(char* fieldname, int lenfield, char* fieldout);

int getehdr( header_in, fieldname, error )
sac_header *header_in;
char      *fieldname;
int          *error;

{
  int i;

  *error = 0;

  for( i=0; i<MIHDR; i++){
    if(!strcmp(fieldname,enum_hdr_fields[i]))break;
  }

  if( i<MIHDR ) return(header_in->ext_ihdr[i]);
  else {
    *error = 1;
    return (IUNDEF);
  }

}
