#include <stdlib.h>
#include <stdio.h>
#include <string.h>

char *strscpy( to, from, l )
char *to;
char *from;
int l;
{
  int len;

  if ((to==NULL) || (from==NULL) || (l <= 0)) return NULL;

  len = strlen(from);
  if ( l < len ) len = l;
 
  strncpy(to,from,len);
 
  to[len] = '\0';

  return to ;
}

