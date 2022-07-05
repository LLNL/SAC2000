#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int backspace(stream,n)
FILE *stream;
int n;
{
   char *buf;
   char *lastchar;

   int  savepos, idx; 
   int saveseek, nread, readpos, 
        savevalue, numread;

   savepos = ftell(stream);
   if(savepos == 0) return(0);

   if((buf=(char *)malloc(savepos+1)) == NULL){
      printf("error allocating memory in backspace\n");
      return (1);
   }
   for( idx = 0 ; idx < savepos+1 ; idx++ ) 
      buf[ idx ] = '\0' ;

   fflush(stream);

   readpos = 0;
   numread = savepos;

   saveseek = fseek(stream,readpos,0);
   nread = fread(buf,sizeof(char),(size_t)numread,stream);

   lastchar = &buf[nread-1];


   for( ; n>0; n--) {
      if(*lastchar == '\n') {lastchar--;savepos--;}
      while ( lastchar >= buf && *lastchar != '\n' ) {
         lastchar--;
         savepos--;
      }
   }


   free(buf);

   if(( savevalue = fseek(stream,savepos,0)) == 0L )
      return(0);
   else return(1);
  

}


