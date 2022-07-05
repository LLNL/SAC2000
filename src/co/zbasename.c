/*
CDOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 *** Function:	zbasename_(name,name_len)  

 *** Purpose:	To return the name of the SAC base directory on a machine.

 *** Returns:	The expanded base name.

 *** Notes:	This program is designed to be called from a f77 routine.

 *** History:   04/19/87	Original version based upon zexpnd.
                04/22/87        Modified to blank fill returned base name.
		02/02/88        Modified to exit if SACAUX is not defined.
           
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CEND
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>



void zbasename(name,name_len)
char name[];
int name_len;

{

    char *temp;
    int i;

    if ((temp = getenv("SACAUX")) != NULL)
      strcpy(name,temp);
    else {
      fprintf(stderr, "ERROR: Environmental variable SACAUX not defined.\n");
      exit(1);
    }

    for(i=strlen(name);i<name_len;i++)
      name[i]=' ';      

    name[name_len-1] = '\0';

    return;
}

            
