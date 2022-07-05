#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void wrhelp(char* ktoken, int ktoken_s, int imode, int lprint, int* nerr)
{
	char kmsg[MCMSG+1], kerase[41];
	char kresp[9], kfilename[ MCPFN + 1 ] ;
	int ncerr, nlw, numsave, nlscrn;
        FILE *nun;
	void zgpmsg();


	/*=====================================================================
	 * PURPOSE:  To write (copy) the contents of a help package to the 
	 *           message subsystem.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ktoken:  Name of help package to write. [c]
	 *    imode:   Write mode [i]:
	 *             = 1 Write entire help file, pausing for each full screen.
	 *             = 2 Write command syntax only.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag.
	 *             Set to 0 if no error occurred.
	 *             Set to -1 if the user wants to abort help command.
	 *             Possible error numbers:  1104, 1105.
	 *=====================================================================
	 * MODULE/LEVEL:  exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MUNOUT
	 *    term:    nlscrn
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  ophelp, setmsg, zclose
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    911218:  Added call to typmsg per Doug Neuhauser email of 9/12/91.
	 *    870924:  Changed error return logic.
	 *    831111:  Removed from machine dependent level.
	 *    830818:  Used NLSCRN from TERM insert to determine full screen.
	 *    830104:  Modifications due to changes in HELP system.
	 *    820823:  Factored from XHELP.
	 *    811116:  Changed check for form-feed from column 1 to column 2.
	 *    810120:  Changed to output message retrieval from disk.
	 *    810115:  Major revision for new help package format.
	 *    800915:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870924
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	nlw = 0;

	/* - Activate automatic output message mode. */

	autooutmsg( TRUE );

	/* - Open requested help package. */

	ophelp( ktoken,ktoken_s, &nun, kfilename , nerr );
	if( *nerr != 0 )
		goto L_8888;
	if( nun == NULL )
		goto L_5000;

	/* If this is the PRINTHELP command, print the file and return, else
	   go on and send it to the monitor one line at a time. */
	if ( lprint ) {
	    char kcommand [ MCPFN + 10 ] ;

	    sprintf ( kcommand , "lpr %s" , kfilename ) ;

	    system ( kcommand ) ;

	    goto L_6000 ;
	}

	/* - Set the message type */

	typmsg( "OUTPUT" );

	/* - Get screen attributes (number of lines per screen and
	 *   text to send to erase screen, if any.) */

	getalphainfo( &nlscrn, kerase,41 );
	if( nlscrn <= 0 )
		nlscrn = 23;

	/* - For each line of text in help package. */

L_3000:
        if(fgets(kmsg,MCMSG+1,nun)==NULL){
		if(feof(nun)) goto L_6000;
		goto L_5000;
	}
        if(kmsg[(numsave=strlen(kmsg)-1)] == '\n') kmsg[numsave]=' ';

	aplmsg( kmsg,MCMSG+1 );
	nlw = nlw + 1;


	/* -- After a screen full of info, see if user wants to see more. */
	if( nlw > (nlscrn - 2) ){
		outmsg();
		clrmsg();
                setmsg("OUTPUT",99);
		zgpmsg( "More? $",8, kresp, 9 );

		upcase( kresp, 1, kresp, 9 );

		if( kresp[0] == 'N' ){
			goto L_6000;
		}
		else if( kresp[0] == 'Q' ){
			/* *nerr = -1; */
			goto L_6000;
		}
		else{
			nlw = 0;
		}
	}

	/* -- Loop back until done. */
	goto L_3000;

	/* - Process error during read. */

L_5000:
	setmsg( "OUTPUT", 1104 );
	apcmsg( ktoken,ktoken_s );
	outmsg();
        clrmsg();

	/* - Close help package and return when:
	 *   (1) end-of-file encountered.
	 *   (2) user requests that remainder of file not be printed. */

L_6000:

	zcloses( &nun, &ncerr );

	/* - Deactivate automatic output message mode. */

	autooutmsg( FALSE );

L_8888:
	return;

} /* end of function */

