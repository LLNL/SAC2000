#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "exm.h"
#include "bbs.h"

void znfiles(FILE** nfu, char *kname, int kname_s, char *ktype, int ktype_s, int* nerr);


void /*FUNCTION*/ xgetbb(nerr)
int *nerr;
{
	char kbbname[MCMSG+1], kbbvalue[MCMSG+1];
	int ic1, ic2, index, nbberr, nc, ncbb ;


	/*=====================================================================
	 * PURPOSE:  To execute the action command GETBB.
	 *           This command reports the values of blackboard variables.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MUNOUT, MCMSG
	 *    bbs:     knmbbs
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    exm:     kbbcl, nunbbwrite, knmbbwrite, lnames, lnewline
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890104:  Changed from terminal output to message subsystem.
	 *    880901:  Added TO, NAMES, and NEWLINE options.
	 *    870917:  Added ALL option.
	 *    870514:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870514
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

	    /* -- "ALL": report all of the currently defined blackboard variables. */
	    if( lckey( "ALL#$",6 ) ){
		cmexm.lbball = TRUE;
	    }

	    /* -- "TO TERMINAL|filename": define where the output is to be sent. */
	    else if( lckey( "TO#$",5 ) ){
		if( lckey( "TERM#INAL$",11 ) ){
		    cmexm.nunbbwrite = MUNOUT;
		}
		else if( lcchar( MCPFN, kmexm.knmbbwrite,MCPFN+1, &nc ) ){
		    cmexm.nunbbwrite = (FILE *)NULL;
		    if( *nerr != 0 )
			goto L_8888;
		}
		else{
		    cfmt( "ILLEGAL OPTION:$",17 );
		    cresp();
		}
	    }

	    /* -- "NAMES ON|OFF": option to include the
				      bb variable name with the value. */
	    else if( lklog( "NAMES#$",8, &cmexm.lnames ) )
	    { /* do nothing */ }

	    /* -- "NEWLINE ON|OFF": option to append newline
				    after each bb variable value. */
	    else if( lklog( "NEWLINE#$",10, &cmexm.lnewline ) )
	    { /* do nothing */ }

	    /* -- The rest of the tokens should be names of a blackboard variables.
	     *    First unset ALL flag and initialize list of bb variables.
	     *    Then set up an inner parsing loop to collect all of the 
	     *    variable names. */
	    else{
		cmexm.lbball = FALSE;
		memset ( kmexm.kbbcl , ' ' , MCMSG );

		while ( lcmore( nerr ) ){
		    if( lcchar( MCMSG, kbbvalue,MCMSG+1, &ncbb ) ){
			putcl( kmexm.kbbcl,MCMSG+1, kbbvalue,MCMSG+1, nerr );
			if( *nerr != 0 )
			    goto L_8888;
		    }
		    else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();
		    }
		}
	    }
	} /* end while */

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
		goto L_8888;

	/* EXECUTION PHASE: */

	/* - Open disk file if necessary. */

	if( cmexm.nunbbwrite != MUNOUT ){
	    znfiles( &cmexm.nunbbwrite, kmexm.knmbbwrite,MCPFN+1, "TEXT",5, 
	     nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    if ( fseek ( cmexm.nunbbwrite , 0L , SEEK_END ) != 0 )
		fprintf ( stdout , "fseek returned error-xgetbb\n" ) ;
	}

	/* - Sequentially access blackboard if ALL was requested. */

	setmsg( "OUTPUT", 99 );
	if( cmexm.lbball ){
	    initvlist( kmbbs.knmbbs,MCPFN+1, &index, nerr );
	    if( *nerr != 0 )
		goto L_8000;

	    while ( encodeventry( &index, kbbname,MCMSG+1, kbbvalue,MCMSG+1 ) ){
		if( cmexm.lnames ){
		    apcmsg( kbbname,MCMSG+1 );
		    apcmsg( "=",2 );
		}
		apcmsg( kbbvalue,MCMSG+1 );
		if( cmexm.lnewline ){
		    if( cmexm.nunbbwrite == MUNOUT ){
			outmsg();
		    }
		    else{
			wrtmsg( cmexm.nunbbwrite );
		    }
		    clrmsg();
		    setmsg( "OUTPUT", 99 );
		}
	    }
	    if( !cmexm.lnewline ){
		if( cmexm.nunbbwrite == MUNOUT ){
		    outmsg();
		}
		else{
		    wrtmsg( cmexm.nunbbwrite );
		}
	    }
	}

	/* - Otherwise, get value for each item in request list. */
	else{
	    ic1 = 0;

	    while ( lnxtcl( kmexm.kbbcl,MCMSG+1, &ic1, &ic2 ) ){
		fstrncpy( kbbname, MCMSG, kmexm.kbbcl+ic1 - 1,min(ic2,MCMSG) - 
		 ic1 + 1);
		getbbv( kbbname, kbbvalue, &nbberr, MCMSG, MCMSG );
		if( cmexm.lnames ){
		    apcmsg( kbbname,MCMSG+1 );
		    apcmsg( "=",2 );
		}
		apcmsg( kbbvalue,MCMSG+1 );
		if( cmexm.lnewline ){
		    if( cmexm.nunbbwrite == MUNOUT ){
			outmsg();
		    }
		    else{
			wrtmsg( cmexm.nunbbwrite );
		    }
		    clrmsg();
		    setmsg( "OUTPUT", 99 );
		}
	    }
	    if( !cmexm.lnewline ){
		if( cmexm.nunbbwrite == MUNOUT ){
		    outmsg();
		}
		else{
		    wrtmsg( cmexm.nunbbwrite );
		}
	    }
	}

L_8000:
	clrmsg();
	if( cmexm.nunbbwrite != MUNOUT ){
	    zcloses( &cmexm.nunbbwrite, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

L_8888:
	return;

} /* end of function */

