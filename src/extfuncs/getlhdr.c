#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "../../inc/extfunc.h"

#ifndef FALSE
#define FALSE (0)
#endif


void getfield(char* fieldname, int lenfield, char* fieldout);


int getlhdr( header_in, fieldname, error )
sac_header *header_in;
char      *fieldname;
int          *error;

{
  int i;

  *error = 0;

  for( i=0; i<MLHDR; i++){
    if(!strcmp(fieldname,log_hdr_fields[i]))break;
  }

  if( i<MLHDR ) return(header_in->ext_lhdr[i]);
  else {
    *error = 1;
    return ((int)FALSE);
  }

}
