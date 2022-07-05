/*
CDOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 *** Function:	zgetgd_(name,name_len)  

 *** Purpose:	To get the name of the default SAC graphics device.

 *** Returns:	The device name.

 *** Notes:	This program is designed to be called from a f77 routine.

 *** History:   11/22/88	Original version based upon zbasename.
           
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CEND
*/

#include <string.h>
#include <stdio.h>
extern char *getenv();


void zgetgd(name,name_len)
char name[];
int name_len;

{

    char *temp;
    int i;

    if ((temp = getenv("SACGRAPHICSDEVICE")) != NULL)
      strcpy(name,temp);
    else {
      strcpy(name,"xwindows");
    }

    for(i=strlen(name);i<name_len;i++)
      name[i]=' ';      

    return;
}

            
