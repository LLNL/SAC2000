#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

int /*FUNCTION*/ non_num_com(command, command_s)
char *command;   int command_s;
{
/* PURPOSE:  To check command against list of commands that do not      */
/*           have numeric args.  For use by ctype, so as not to convert */
/*           numeric strings to numbers if not appropriate.             */

  if(     memcmp(command,"r "      ,2) == 0) return (int)TRUE;
  else if(memcmp(command,"read "   ,5) == 0) return (int)TRUE;
  else if(memcmp(command,"rh "     ,3) == 0) return (int)TRUE;
  else if(memcmp(command,"readhdr ",8) == 0) return (int)TRUE;
  else if(memcmp(command,"readcss ",8) == 0) return (int)TRUE;
  else if(memcmp(command,"rcss "   ,5) == 0) return (int)TRUE;
  else if(memcmp(command,"merge "  ,6) == 0) return (int)TRUE;
  else if(memcmp(command,"setbb "  ,6) == 0) return (int)TRUE;

  return (int)FALSE;  

}

