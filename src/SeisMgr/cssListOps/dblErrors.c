#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dblErrors.h"


static char *ErrorMessage=0;
static int NumErrors;



void dblClearErrorList(void)
{
  char header[] = "Summary of current errors:\n";
  if(ErrorMessage)free(ErrorMessage);
  ErrorMessage = (char *) malloc(strlen(header) +1);
  strcpy(ErrorMessage,header);
  NumErrors = 0;
}
/*----------------------------------------------------------------------*/






void dblSetError(int Severe, const char * string)
{
  int errmsgLen = 0;
  int j;

  if(string){
    if(ErrorMessage) { 
	errmsgLen = strlen(ErrorMessage);
    }
    ErrorMessage = (char *) realloc(ErrorMessage, 
				    (errmsgLen + strlen(string) +2) * sizeof(char) );
    if ( !errmsgLen ) {
        strcpy(ErrorMessage,string);
    }
    else {
        strcat(ErrorMessage,string);
    }
    strcat(ErrorMessage,"\n");
  }
  NumErrors++;

  return;
}
/*----------------------------------------------------------------------*/






char *dblGetErrorMessage(void)
{
  return ErrorMessage;
}




int dblGetNumErrors(void)
{
  return NumErrors;
}
/*----------------------------------------------------------------------*/

