#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MTEMP	1000

#include "../../inc/mach.h"
void /*FUNCTION*/ xsetbb(nerr)
int *nerr;
{
	char kname[MCMSG+1], ktemp[1001], kvalue[MCMSG+1];
	int lappend;
	int nc1, nc2, nchar, ntused;
	float fvalue;
        char *strtemp;

        int malloc_verify();

	/*=====================================================================
	 * PURPOSE:  To execute the action command SETBB.
	 *           This command sets or defines blackboard variable values.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCMSG
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCCHAR, SETBBV
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    kname:   Name of blackboard variable. [c]
	 *    kvalue:  Value of blackboard variable. [c]
	 *    ktemp:   Used to create value field when in append mode. [c]
	 *    mtemp:   Length of ktemp. [ip]
	 *             Should be the same size as "mcl" parameter in xdo.f
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871109:  Added option to append text to a blackboard variable.
	 *    870904:  Fixed bug with numbers greater than a token in width.
	 *    870301:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870301
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

	    /* -- "name [APPEND] value":  the name and value of the blackboard variable.
	     *    Store the name and value in the blackboard store. */
	    if( lcchar( MCMSG, kname,MCMSG+1, &nchar ) ){
		if( lckey( "APPEND#$",9 ) ){
		    lappend = TRUE;
		}
		else{
		    lappend = FALSE;
		}
L_1200:
		if( lcchar( MCMSG, kvalue,MCMSG+1, &nchar ) ){
		    /* --- Next if clause is temporary fix to problem in cpf
		     *     that limits floating point fields to one token.
		     *     Remove when ncpf has been completed.  */
		    if( kvalue[nchar - 1] == '$' ){
			ictok( -1 );
			lcreal( &fvalue );
                        sprintf(kvalue,"%16.5g",fvalue);
			ictok( 1 );
		    }
		    if( lappend ){
			fstrncpy( ktemp, 1000, " ", 1 );
			getbbv( kname,ktemp, &ntused, MCMSG, 1000 );
			nc1 = indexb( ktemp,1001 );
			nc2 = indexb( kvalue,MCMSG+1 );
			if( (nc1 + nc2) > MTEMP ){
			    *nerr = 922;
			    setmsg( "ERROR", *nerr );
			    apimsg( MTEMP );
			    goto L_8888;
			}

                        strtemp = malloc(nc2+1);
                        strncpy(strtemp,kvalue,nc2);
                        strtemp[nc2] = '\0';

			subscpy( ktemp, nc1, nc1 + nc2 - 1, 1000, strtemp);

                        free(strtemp);
			setbbv( kname, ktemp, nerr, MCMSG, 1000 );
		    }
		    else{
#ifdef DEBUG
     malloc_verify();
#endif                                     
			setbbv( kname, kvalue, nerr, MCMSG, MCMSG );
#ifdef DEBUG
     malloc_verify();
#endif                                     

		    }
		    if( *nerr != 0 )
			goto L_8888;
		}
		else{
		    if( nchar <= MCPW ){
			ictok( -1 );
		    }
		    else{
			ictok( -((nchar - 1)/MCPW + 3) );
		    }
		    cfmt( "NEED A BLACKBOARD VALUE",24 );
		    cresp();
		    if( lcmore( nerr ) )
			goto L_1200;
		}

	    }
	    else{
		/* -- Bad syntax. */
		cfmt( "ILLEGAL OPTION:$",17 );
		cresp();
	    }
	}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

} /* end of function */

