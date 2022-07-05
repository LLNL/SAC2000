#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ reprtw(ktext, ktext_s, lrtw, krtw, krtw_s, ortw)
char *ktext;   int ktext_s;
int lrtw;
char *krtw;   int krtw_s;
float ortw[];
{
#define KRTW(I_,J_)	(krtw+(I_)*(krtw_s)+(J_))
	char kline[MCMSG+1];
        char chartemp[3];


	float *const Ortw = &ortw[0] - 1;


	/*=====================================================================
	 * PURPOSE: To report the value of a "relative time window".
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ktext:   Text to accompany value of variable. [c]
	 *    lrtw:    Logical flag.  .TRUE. if "rtw" is on. [l]
	 *    krtw:    Character array giving starting and
	 *             stopping reference times. [ka]
	 *    ortw:    Floating point array giving starting and
	 *             stopping offsets in seconds. [fa]
	 *=====================================================================
	 * MODULE/LEVEL:  exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  replv, aplmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890104:  Changed from direct terminal output to message subsystem.
	 *    830121:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890104
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Report logical variable with text. */
	replv( ktext,ktext_s, lrtw );

	/* - Report stop and stop window if on. */

	if( lrtw ){
                strcpy(chartemp,"  ");
                memcpy(chartemp,KRTW(0,0),2);
                sprintf(kline,"   %s%s%12.5g", "Start is ", chartemp, Ortw[1] );
		aplmsg( kline,MCMSG+1 );

                memcpy(chartemp,KRTW(1,0),2);
                sprintf(kline,"   %s%s%12.5g", "Stop  is ", chartemp, Ortw[2] );
		aplmsg( kline,MCMSG+1 );
		}

L_8888:
	return;

#undef	KRTW
} /* end of function */

