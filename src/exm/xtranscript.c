#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "exm.h"
#include "msg.h"

void znfiles(FILE** nfu, char *kname, int kname_s, char *ktype, int ktype_s, int* nerr);

void /*FUNCTION*/ xtranscript(nerr)
int *nerr;
{
	char kmessage[MCMSG+1], kname[MCPFN+1];
	int lcontents[MTPMSG], lnewcontents, lnewmessage, lnewname;
	int jdx, jtpmsg, nchar, nderr ;


	/*=====================================================================
	 * PURPOSE: To parse the action command TRANSCRIPT.
	 *          This command controls output to the transcript file.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  exm/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCPFN, MCMSG
	 *    msg:     MTPMSG, ktpmsg
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    exm:     knametranscript, nuntranscript, imodetranscript,
	 *             itranscript
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, lckey, lkchar, lkentries,cfmt, cresp, 
	 *             zgtfun, znfile, zclose, zdest, sendmesg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    891128:  Added "FLUSH" option.
	 *    890117:  Added message option and changed some variable names.
	 *    881230:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881230
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Initialize parsing flags. */

	lnewcontents = FALSE;
	lnewmessage = FALSE;
	lnewname = FALSE;

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

	    /* -- "OPEN":  Open a (possibly existing) transcript file and
			   position to the bottom of it
			   (i.e. append to file.) */
	    if( lckey( "OPEN$",6 ) ){
		cmexm.imodetranscript = 1;
	    }

	    /* -- "CREATE":  Create a new transcript file. */
	    else if( lckey( "CR#EATE$",9 ) ){
		cmexm.imodetranscript = 2;
	    }

	    /* -- "CLOSE":  Close a current transcript file. */
	    else if( lckey( "CL#OSE$",8 ) ){
		cmexm.imodetranscript = 3;
	    }

	    /* -- "CHANGE": Change the contents of a current transcript file */
	    else if( lckey( "CH#ANGE$",9 ) ){
		cmexm.imodetranscript = 4;
	    }

	    /* -- "WRITE":  Don't change status of transcript file.
			    Just write message to it. */
	    else if( lckey( "W#RITE$",8 ) ){
		cmexm.imodetranscript = 5;
	    }

	    /* -- "FLUSH":  Flush the buffer associated with a
			    current transcript file. */
	    else if( lckey( "FL#USE$",8 ) ){
		cmexm.imodetranscript = 6;
	    }

	    /* -- "FILE filename":  Set name of transcript file. */
	    else if( lkchar( "FILE$",6, MCPFN, kname,MCPFN+1, &nchar ) ){
		lnewname = TRUE;
	    }

	    /* -- "CONTENTS list": Define the contents of this transcript file*/
	    else if( lkentries( "CO#NTENTS$",11, (char*)kmmsg.ktpmsg,9, 
		     MTPMSG, lcontents ) ){
		lnewcontents = TRUE;
	    }

	    /* -- "MESSAGE text":  Write this message to transcript file. */
	    else if( lkchar( "MESSAGE$",9, MCMSG, kmessage,MCMSG+1, &nchar ) ){
		lnewmessage = TRUE;
	    }

	    /* -- Bad syntax. */
	    else{
		cfmt( "ILLEGAL OPTION:$",17 );
		cresp();
	    }
	}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	/* EXECUTION PHASE: */

	/* - Search list of active transcript files for this entry if name was given.
	 *   Create a new entry if necessary. */

	if( lnewname ){
	    jdx = 1;
L_2000:
	    if( memcmp(kname,kmexm.knametranscript[jdx - 1], min(strlen(kname),
		strlen(kmexm.knametranscript[jdx - 1]))) == 0 ){
		if( lnewcontents ){
		    for( jtpmsg = 0; jtpmsg < MTPMSG; jtpmsg++ ){
			cmexm.lsendtranscript[jdx - 1][jtpmsg] =
			      lcontents[jtpmsg];
		    }
		}
	    }
	    else if( jdx < cmexm.ntranscripts ){
		jdx = jdx + 1;
		goto L_2000;
	    }
	    else if( jdx < MTRANSCRIPTS ){
		if( cmexm.ntranscripts > 0 )
		    jdx = jdx + 1;
		strcpy( kmexm.knametranscript[jdx - 1], kname );
		if( lnewcontents ){
		    for( jtpmsg = 0; jtpmsg < MTPMSG; jtpmsg++ ){
			cmexm.lsendtranscript[jdx - 1][jtpmsg] =
			      lcontents[jtpmsg];
		    }
		}
		else{
		    for( jtpmsg = 0; jtpmsg < MTPMSG; jtpmsg++ ){
			cmexm.lsendtranscript[jdx - 1][jtpmsg] = TRUE;
		    }
		}
		cmexm.ntranscripts = cmexm.ntranscripts + 1;
	    }
	    else{
		*nerr = 1118;
		apimsg( MTRANSCRIPTS );
		goto L_8888;
	    }
	    cmexm.itranscript = jdx;
	}

	/* - If name was not given, assume the previous transcript file. */
	else{
	    jdx = cmexm.itranscript;
	}

	/* - If OPEN (imodetranscript=1) was requested:
	 *   (1) Get an unused fortran file unit.
	 *   (2) Use "znfile" to open (or create if necessary) transcript file.
	 *   (3) Position to the end of the file.
	 *   (4) Activate message output to this file.
	 *   (5) Write message to file if requested. */

	if( cmexm.imodetranscript == 1 ){
	    znfiles( &cmexm.nuntranscript[jdx-1],
		     (char*)kmexm.knametranscript[jdx - 1] ,
		     MCPFN+1, "TEXT",5, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    if ( fseek ( cmexm.nuntranscript[jdx-1] , 0L , SEEK_END ) != 0 )
		fprintf ( stdout , "fseek returned error-xtranscript\n" ) ;
	    sendmesg( cmexm.nuntranscript[jdx-1], TRUE,
		      &cmexm.lsendtranscript[jdx - 1][0] );
	    if( lnewmessage ){
		setmsg( "OUTPUT", 99 );
		apcmsg( kmessage,MCMSG+1 );
		wrtmsg( cmexm.nuntranscript[jdx-1] );
		clrmsg();
	    }
	}


	/* - If CREATE (imodetranscript=2) was requested:
	 *   (1) Get an unused fortran file unit.
	 *   (2) Destroy existing file by this name.
	 *       (Ignore error return which means file did not exist.)
	 *   (3) Use znfile to create a new transcript file.
	 *   (4) Activate message output to this file.
	 *   (5) Write message to file if requested. */

	else if( cmexm.imodetranscript == 2 ){
	    zdest( (char*)kmexm.knametranscript[jdx - 1],MCPFN+1, &nderr );
	    if( nderr != 0 )
		clrmsg();
	    znfiles( &cmexm.nuntranscript[jdx-1],
		     (char*)kmexm.knametranscript[jdx - 1] ,
		     MCPFN+1, "TEXT",5, nerr );
	    if( *nerr != 0 )
		goto L_8888;

	    sendmesg( cmexm.nuntranscript[jdx-1], TRUE,
		      &cmexm.lsendtranscript[jdx - 1][0] );
	    if( lnewmessage ){
		setmsg( "OUTPUT", 99 );
		apcmsg( kmessage,MCMSG+1 );
		wrtmsg( cmexm.nuntranscript[jdx-1] );
		clrmsg();
	    }
	}


	/* - If CLOSE (imodetranscript=3) was requested:
	 *   (1) Write message to file if requested.
	 *   (2) Close transcript file.
	 *   (3) Deactivate message output to this file.
	 *   (4) Zero the fortran file unit number storage so that
	 *       we know that the file is currently closed. */

	else if( cmexm.imodetranscript == 3 ){
	    if( lnewmessage ){
		setmsg( "OUTPUT", 99 );
		apcmsg( kmessage,MCMSG+1 );
		wrtmsg( cmexm.nuntranscript[jdx-1] );
		clrmsg();
	    }
	    zcloses( &cmexm.nuntranscript[jdx-1], nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    sendmesg( cmexm.nuntranscript[jdx-1], FALSE,
			&cmexm.lsendtranscript[jdx - 1][0] );
	    cmexm.nuntranscript[jdx-1] = 0;
	}


	/* - If CHANGE (imodetranscript=4) and file is open, send change to the
	 *   message handling subsystem.  If file is currently closed, simply
	 *   store change until the next time this transcript file is opened.
	 *   Write message to file if requested. */

	else if( cmexm.imodetranscript == 4 ){
	    if( cmexm.nuntranscript[jdx-1] != 0 ){
		sendmesg( cmexm.nuntranscript[jdx-1], TRUE,
			  &cmexm.lsendtranscript[jdx - 1][0] );
	    }
	    if( lnewmessage ){
		setmsg( "OUTPUT", 99 );
		apcmsg( kmessage,MCMSG+1 );
		wrtmsg( cmexm.nuntranscript[jdx-1] );
		clrmsg();
	    }
	}

	/* - If WRITE(imodetranscript=5) just write message to transcript file*/
	else if( cmexm.imodetranscript == 5 ){
	    if( lnewmessage ){
		setmsg( "OUTPUT", 99 );
		apcmsg( kmessage,MCMSG+1 );
		wrtmsg( cmexm.nuntranscript[jdx-1] );
		clrmsg();
	    }
	}


	/* - If FLUSH (imodetranscript=6) was requested:
	 *   (1) Close the transcript file to flush the buffer.
	 *   (2) Use "znfile" to reopen transcript file.
	 *   (3) Position to the end of the file.
	 *   (4) Activate message output to this file.
	 *   (5) Write message to file if requested. */

	else if( cmexm.imodetranscript == 1 ){
	    zcloses( &cmexm.nuntranscript[jdx-1], nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    znfiles( &cmexm.nuntranscript[jdx-1],
		     (char*)kmexm.knametranscript[jdx - 1] ,
		     MCPFN+1, "TEXT",5, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    if ( fseek ( cmexm.nuntranscript[jdx-1] , 0L , SEEK_END ) != 0 )
		fprintf ( stdout , "fseek returned error-xtranscript\n" ) ;
	    sendmesg( cmexm.nuntranscript[jdx-1],
			TRUE, &cmexm.lsendtranscript[jdx - 1][0] );
	    if( lnewmessage ){
		setmsg( "OUTPUT", 99 );
		apcmsg( kmessage,MCMSG+1 );
		wrtmsg( cmexm.nuntranscript[jdx-1] );
		clrmsg();
	    }
	}

L_8888:
	return;

} /* end of function */

