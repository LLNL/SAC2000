#include <string.h>
#include "../../inc/extfunc.h"


void getfield(char* fieldname, int lenfield, char* fieldout);

void fsetahdr_(hdr_index, fieldname, value, error, lenfield, lenvalue)
int *hdr_index;
char *fieldname;
char     *value;
int     *error;
int   lenfield;
int   lenvalue;
{
  char infield[8];
  char invalue[17];
  sac_header *hdr_in;

  hdr_in = indata->ext_hdrs[*hdr_index-1];

  memset(infield,' ',8);
  infield[7]='\0';
  memset(invalue,' ',17);
  invalue[16]='\0';

  getfield(fieldname, lenfield, infield);
  getfield(value, lenvalue, invalue);

  setahdr(hdr_in, infield, invalue, error);

  return;
  
}
