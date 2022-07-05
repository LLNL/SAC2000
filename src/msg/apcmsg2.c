/*
  Routine to copy substrings into null terminated strings to
  pass to apcmsg.

  L. Minner
  9/8/93
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/msg.h"

void apcmsg2(char* kalpha, int kalpha_s)
{
        char message[MCMSG];
        
        int msglen = (kalpha_s < (MCMSG - 1)) ? kalpha_s : (MCMSG - 1);

        strncpy(message,kalpha,msglen);
        message[msglen] = '\0';

        apcmsg(message,msglen + 1);

        return;

} /* end of function */

