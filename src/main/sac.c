#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/exm.h"
#include "../../inc/history.h"

int main( argc, argv )
int argc; char *argv[];
{
	char kline[MCMSG+1], kmsg[MCMSG+1];
	int ic, ic1, ic2, idx, itype, nc, ncline, ncmsg, nerr, i;
	void zgpmsg();
        char *s1, *s2;
         int lgui = FALSE , dumb ;

	/*=======================================================================
	 * PURPOSE:  Main command execution loop for SAC.
	 *=======================================================================
	 * MODULE/LEVEL:  exm/0
	 *=======================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG
	 *    exm:     kprmt
	 *=======================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  initsac, executemacro, zgpmsg, processline,
	 *             saccommands, setmsg, reperr, proerr
	 *=======================================================================
	 * LOCAL VARIABLES:
	 *    kmsg:    Message received from terminal. [c]
	 *    ncmsg:   Length of KMSG without trailing blanks. [i]
	 *    kline:   Message after being processed to substitute blackboard
	 *             and header variables. [c]
	 *    ncline:  Length of KLINE without trailing blanks. [i]
	 *    nerr:    Error return flag. [i]
	 *             =0 if ok.
	 *             >0 if message was invalid or command encountered errors.
	 *=======================================================================
	 * MODIFICATION HISTORY:
	 *    900804:  Minor change in subroutine names.
	 *    891005:  General cleanup: included call to zstart for system
	 *             dependent startup code; deleted some obsolete code;
	 *             changed calling logic to saccommands.
	 *    870421:  Added call to processline.
	 *    870109:  Changed to zgpmsg to handle "graphics events."
	 *    841219:  Retyped on a ROS3.2/RIDGE 32, removed interrupt control.
	 *    820923:  Added interrupt control and documented subroutine.
	 *    820430:  Mod due to change in ZGPMSG.
	 *    810000:  Original version.
	 *=======================================================================
	 * DOCUMENTED/REVIEWED:  891005
	 *======================================================================= */
	/* PROCEDURE: */

	/* initialize kmsg.  maf 970630 */
	for( idx = 0 ; idx < MCMSG ; idx++ ) {
	    kmsg[ idx ] = ' ' ;
            kline[ idx ] = ' ' ;
	}
	kmsg[0] = '\0' ;
	kmsg[MCMSG] = '\0' ;
	kline[MCMSG] = '\0' ;
#ifdef DEBUG
        malloc_debug(2);
#endif                

	/* - Initialize common. */
	initsac();

        /* check for "gui" execute line arg. */
        for( i=1; i<argc; i++ ){
            if(strncmp(argv[i],"gui",3) == 0){
		strncpy(argv[i],"   ",3); /* blank out the gui arg so it's not */
		lgui = TRUE;              /* confused with a macro to execute. */
		break;
	    }
	}

        /* start up the gui. */
        /* if( lgui ) gsac(argc,argv); */ /* this call never returns. */

	/* - Get the input line message, if any.
	 *   This should be the name of the a default SAC macro to execute. */

	zgimsg(argc,argv,kmsg,MCMSG+1 );
	nc = indexb( kmsg,MCMSG+1 );
	if( nc > 0 ){
	    ic = 0;
	    poptok( kmsg, nc, &ic, &ic1, &ic2, &itype );
	    setmsg( "COMMAND", 99 );
	    apcmsg( "(INPUT LINE) MACRO",19 );
	    apcmsg( kmsg,MCMSG+1 );
	    outmsg();
	    clrmsg();

            strncpy((s1=malloc(ic2-ic1+2)),kmsg+ic1 - 1,ic2-ic1+1);
            s1[ic2-ic1+1] = '\0';
            strncpy((s2=malloc(nc-ic+2)),kmsg+ic - 1,nc-ic+1);
            s2[nc-ic+1] = '\0';

	    executemacro( s1, ic2-ic1+2, s2, nc-ic+2, &nerr );

	    free(s1);
	    free(s2);
	}

	/* - THIS IS THE MAIN LOOP OF THE PROGRAM.
	 *   (1) "zgpmsg" sends a prompt to the user and gets a message back.
	 *   (2) The message is first passed through "processline" which evaluates any
	 *       blackboard or header variables and substitutes them into the message.
	 *   (3) The message is then passed to "saccommands" which parses and executes
	 *       the command(s) that are contained within the message.
	 *   (4) When all the commands have been executed or an error has
	 *       occured, "saccommands" returns and the process is repeated.
	 *   (5) Program termination is handled by the QUIT command found
	 *       in the Executive Module (subroutine "xexmc"). */

	/* the main loop of the program */
	while( TRUE ){
	    zgpmsg( kmexm.kprmt,13, kmsg,MCMSG+1 );
	    strcpy(kmsg,AddToHistory(kmsg)) ;
	    ncmsg = indexb( kmsg,MCMSG+1 );
	    if( ncmsg >= MCMSG ){
		setmsg( "ERROR", 99 );
		apcmsg("Error: Cmd line exceeds buffer limited to num of chars: "
			,57 );
		apimsg( MCMSG );
		outmsg();
		clrmsg();
		continue ;
	    }
	    setmsg( "COMMAND", 99 );
	    apcmsg( kmsg,MCMSG+1 );	/* Add message to the list in kmmsg */
	    outmsg();		/* Write message to appropriate output devices */
	    clrmsg();		/* Clear message from list in kmmsg */
	    /* processline: strip the message of info re: black board vars,
	     * header vars, arguments, escapes, and inline functions, and write
	     * the result to kline. */


	    processline( "terminal", 9, kmsg, MCMSG+1, ncmsg, kline, MCMSG+1,
	             &ncline, &nerr );

	    if( nerr != 0 ){
		reperr( nerr );
		if( nerr != 0 )
		    proerr( &nerr );
		continue ;
	    }
	    dumb = ( strlen( kline ) < strlen( kmsg ) ?
		     strlen( kline ) : strlen( kmsg ) ) ;

	    /* if processline() had an effect */
	    if( !dumb || memcmp(kline,kmsg,dumb) != 0 ){
		setmsg( "PROCESSED", 99 );	     /* let it be noted. */
		apcmsg( kline,MCMSG+1 );
		outmsg();
		clrmsg();
	    }
	    if( ncline > 0 )
		/* saccommands:  executes the commands in kline. */
		saccommands( kline,MCMSG+1, &nerr );
	} /* end while (the main loop in the program */

} /* end of function */

