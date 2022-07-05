#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "../../inc/extfunc.h"

void getfield(char* fieldname, int lenfield, char* fieldout);

void setehdr( header_in, fieldname, value, error )
sac_header *header_in;
char      *fieldname;
int           value;
int          *error;

{
  int i;

  *error = 0;

  for( i=0; i<MIHDR; i++){
    if(!strcmp(fieldname,enum_hdr_fields[i]))break;
  }

  if( i<MIHDR ) header_in->ext_ihdr[i] = value;
  else *error = 1;

  return;


}
