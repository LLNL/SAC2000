#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/exm.h"
void /*FUNCTION*/ tracevariable(activate, blackboard, variable, nerr)
int activate, blackboard;
char *variable;
int *nerr;
{
	int j, j_, jtrace, jtrace_, ngerr, ntracesav;



	/*=====================================================================
	 * PURPOSE: To control the tracing of a blackboard or header variable.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    activate:    Set to .TRUE. to activate tracing of variable.
	 *                 Set to .FALSE. to deactivate tracing of variable. [l]
	 *    blackboard:  Set to .TRUE. if variable to trace is a blackboard
	 *                 variable. Set to .FALSE. if a header variable. [l]
	 *    variable:    Name of the variable to trace. [c]
	 *                 If this is the name of a header variable then
	 *                 it must be of the form: "file,name" where "file"
	 *                 is the data file list name or number and "name"
	 *                 is the SAC header variable name. 
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *     nerr:    Error flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    
	 *    exm:      MTRACES
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    exm:      ntraces, ktracename, ktracevalue, lblackboard
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

	ntracesav = cmexm.ntraces;
	if( activate ){
		jtrace = 1;
L_100:
		if( memcmp(variable,cmexm.ktracename[jtrace - 1],
                    min(strlen(variable),strlen(cmexm.ktracename[jtrace - 1]))) == 0 ){
			}
		else if( jtrace < cmexm.ntraces ){
			jtrace = jtrace + 1;
			goto L_100;
			}
		else if( cmexm.ntraces < MTRACES ){
			cmexm.ntraces = cmexm.ntraces + 1;
			jtrace = cmexm.ntraces;
			fstrncpy( cmexm.ktracename[jtrace - 1], 16,  variable, strlen(variable));
			Lblackboard[jtrace] = blackboard;
			if( Lblackboard[jtrace] ){
				getbbv( (char*)cmexm.ktracename[jtrace - 1], (char*)cmexm.ktracevalue[jtrace - 1]
				 , &ngerr, 16, MCMSG );
				}
			else{
				gethv( (char*)cmexm.ktracename[jtrace - 1],17, (char*)cmexm.ktracevalue[jtrace - 1]
				 ,MCMSG+1, &ngerr );
				}
			setmsg( "OUTPUT", 99 );
			apcmsg( "TRACE  (on)",12 );
			apcmsg( (char*)cmexm.ktracename[jtrace - 1],17 );
			apcmsg( "=",2 );
			apcmsg( (char*)cmexm.ktracevalue[jtrace - 1],MCMSG+1 );
			outmsg();
			clrmsg();
			}
		else{
			*nerr = 1119;
			setmsg( "ERROR", *nerr );
			apimsg( MTRACES );
			goto L_8888;
			}
		}
	else{
		for( jtrace = 1; jtrace <= cmexm.ntraces; jtrace++ ){
			jtrace_ = jtrace - 1;
			if( memcmp(variable,cmexm.ktracename[jtrace_],
                            min(strlen(variable),strlen(cmexm.ktracename[jtrace_]))) == 0 ){
				if( Lblackboard[jtrace] ){
					getbbv( (char*)cmexm.ktracename[jtrace_], (char*)cmexm.ktracevalue[jtrace_]
					 , &ngerr, 16, MCMSG );
					}
				else{
					gethv( (char*)cmexm.ktracename[jtrace_],17, (char*)cmexm.ktracevalue[jtrace_]
					 ,MCMSG+1, &ngerr );
					}
				setmsg( "OUTPUT", 99 );
				apcmsg( "TRACE (off)",12 );
				apcmsg( (char*)cmexm.ktracename[jtrace_],17 );
				apcmsg( "=",2 );
				apcmsg( (char*)cmexm.ktracevalue[jtrace_],MCMSG+1 );
				outmsg();
				clrmsg();
				ntracesav = cmexm.ntraces - 1;
				for( j = jtrace; j <= ntracesav; j++ ){
					j_ = j - 1;
					Lblackboard[j] = Lblackboard[j + 1];
					strcpy( cmexm.ktracename[j_], cmexm.ktracename[j_ + 1]
					  );
					strcpy( cmexm.ktracevalue[j_], cmexm.ktracevalue[j_ + 1]
					  );
					}
				goto L_8888;
				}
			}
		}

L_8888:
	cmexm.ntraces = ntracesav;
	return;

} /* end of function */

