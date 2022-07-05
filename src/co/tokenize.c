#include <stdlib.h>
#include <string.h>
/* Tokenize a character string. */

void tokenize(argv, argc, linein, nerr)
char ***argv;
int    *argc;
char *linein;
int   *nerr;
{
  char *temp, *token;
  int i;

  *nerr = 0;
  *argc = 0;


  if((temp = malloc(strlen(linein)+1)) == NULL){
    *nerr = 301;
    return;
  }
  
  strcpy(temp, linein);

  token = strtok(temp, " \t\n\0");
  if( token != NULL ) (*argc)++;

  while( (token = strtok(NULL, " \t\n\0")) != NULL ) (*argc)++;

  if( *argc > 0 ){
    if((*argv = malloc(*argc * sizeof(char *))) == NULL){
      *nerr = 301;
      return;
    }else{
      (*argv)[0] = strtok(linein, " \t\n\0");
      for( i=1; i<*argc; i++ ){
        (*argv)[i] = strtok(NULL, " \t\n\0");
      }
    }

  }
  
  free(temp);

  return;

}
