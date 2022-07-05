#include <string.h>
#include "../../inc/extfunc.h"

void getfield(char* fieldname, int lenfield, char* fieldout);

void fgetnhdr_(hdr_index, fieldname, value, error, lenfield)
int *hdr_index;
char *fieldname;
int     *value;
int     *error;
int   lenfield;
{
  char infield[8];
  sac_header *hdr_in;

  hdr_in = indata->ext_hdrs[*hdr_index-1];

  memset(infield,' ',8);
  infield[7]='\0';

  getfield(fieldname, lenfield, infield);

  *value = getnhdr(hdr_in, infield, error);

  return;
}
