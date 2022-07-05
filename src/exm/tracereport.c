#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/exm.h"
void /*FUNCTION*/ tracereport(nerr)
int *nerr;
{
	char currentvalue[MCMSG+1];
	int jtrace, jtrace_, ngerr;



	/*=====================================================================
	 * PURPOSE: To send a variable tracing report to the message subsystem.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *     nerr:    Error flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    
	 *    exm:      ntraces, ktracename, ktracevalue, lblackboard
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    exm:      ktracevalue
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:   getbbv, gethv, setmsg, apcmsg, apimsg, outmsg, clrmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881230:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881230
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	for( jtrace = 1; jtrace <= cmexm.ntraces; jtrace++ ){
		jtrace_ = jtrace - 1;
		if( Lblackboard[jtrace] ){
			getbbv( (char*)cmexm.ktracename[jtrace_], currentvalue
			 , &ngerr, 16, MCMSG );
			}
		else{
			gethv( (char*)cmexm.ktracename[jtrace_],17, currentvalue
			 ,MCMSG+1, &ngerr );
			}
		if( memcmp(currentvalue,cmexm.ktracevalue[jtrace_],min(strlen(currentvalue),
                                           strlen(cmexm.ktracevalue[jtrace_]))) != 0
		  ){
			strcpy( cmexm.ktracevalue[jtrace_], currentvalue );
			setmsg( "OUTPUT", 99 );
			apcmsg( "TRACE (mod)",12 );
			apcmsg( (char*)cmexm.ktracename[jtrace_],17 );
			apcmsg( "=",2 );
			apcmsg( (char*)cmexm.ktracevalue[jtrace_],MCMSG+1 );
			outmsg();
			clrmsg();
			}
		}

L_8888:
	return;

} /* end of function */

