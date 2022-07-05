#include <stdio.h>
#include <strings.h>
#include <string.h>
#include "mach.h"
#include "msg.h"

/* Report version information about SAC2000 */

void setmsg(char* ktype, int number);
void apcmsg(char *kalpha, int kalpha_s);
void outmsg(void);
void clrmsg(void);
void aplmsg(char *kalpha, int kalpha_s);



void xabout ( void ) 
{
    char kvdate[200];
    char fmt[] = "SEISMIC ANALYSIS CODE [%s (Version 07.10.8)]";
    char kcopyr[] = "Copyright 2019 LLNL/LLNS under Contract DE-AC52-07NA27344\n" ;

    sprintf( kvdate, fmt, BUILD_DATE );
    setmsg( "OUTPUT", 99 );
    apcmsg( kvdate, strlen ( kvdate ) + 1 );
    aplmsg( kcopyr, strlen ( kcopyr ) + 1 );
    outmsg();
    clrmsg();
}

