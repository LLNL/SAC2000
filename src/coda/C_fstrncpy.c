#include <stdlib.h>
#include <stdio.h>
#include <string.h>


char *C_fstrncpy( char* to, int tolen, char* from, int fromlen )
{
   int cpylen;

   if ( to==NULL || from==NULL || 
        tolen <= 0 || fromlen <= 0 ) return(NULL);

   cpylen = fromlen;
   if ( fromlen > tolen ) cpylen = tolen;

   memcpy(to,from,cpylen);
   if(cpylen < tolen) memset(to+cpylen,(int)' ',tolen-cpylen);
   to[tolen] = '\0';

   return( to );
}
