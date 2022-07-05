#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"

void apcmsg2(char* kalpha, int kalpha_s);
int lckeyExact(char* kkey,int kkey_s);
void alignFiles ( int *nerr );




void /*FUNCTION*/ xwh(nerr)
int *nerr;
{
	int ic1, ic2, icomORroll, jdfl, ndx1, ndx2, nlen;
        char *strtemp;


	/*=====================================================================
	 * PURPOSE:  To execute the action command WRITEHDR.
	 *           This command writes updated headers to disk.
	 *=====================================================================
	 * INPUT ARGUMENTS: None.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1303, 1341
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     LOVROK, FMT
	 *    DFM:     NDFL, KDFL, LCUT
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  VFLIST, SETMSG, GETFIL
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */
	if ( lcmore ( nerr ) ) {
	    /* -- "COMMIT|RECALLTRACE|ROLLBACK": how to treat existing data */
	    if ( lckeyExact ( "COMMIT" , 7 ) )
		cmdfm.icomORroll = COMMIT ;
	    else if (lckeyExact ( "RECALLTRACE" , 12 ) )
		cmdfm.icomORroll = RECALL ;
	    else if ( lckeyExact ( "RECALL" , 7 ) )
		cmdfm.icomORroll = RECALL ;
	    else if ( lckeyExact ( "ROLLBACK" , 9 ) )
		cmdfm.icomORroll = ROLLBACK ;
	}


	/* CHECKING PHASE: */

	/* - Check for null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Make sure CUT is off. */

	if( cmdfm.lcut ){
		*nerr = 1341;
		setmsg( "ERROR", *nerr );
		goto L_8888;
	}

	/* EXECUTION PHASE: */

        alignFiles ( nerr ) ;
	if ( *nerr )
	    return ;


	/* - For each file in DFL: */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		/* -- Get header from working memory or disk determine file name. */
		getfil( jdfl, FALSE, &nlen, &ndx1, &ndx2, nerr );
		if( *nerr != 0 )
			goto L_8888;
		lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );

		/* -- Check to see if overwrite flag is enabled. */
		if( !*lovrok ){
			*nerr = 1303;
			setmsg( "ERROR", *nerr );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			goto L_8888;
		}

		/* -- Write header. */
                strtemp = malloc(ic2-ic1+2);
                strncpy(strtemp,kmdfm.kdfl+ic1-1, ic2-ic1+1);
                strtemp[ic2-ic1+1] = '\0';

                             
                
		wrsac( jdfl, strtemp, ic2-ic1+2, FALSE, 1, nerr );

                free(strtemp);

		if( *nerr != 0 )
			goto L_8888;

	}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    850801:  Deleted SOCKITOME format.
	 *    830930:  Added error condition when CUT option is on.
	 *    820721:  Changed to newest set of parsing and checking functions.
	 *    801003:  Original version.
	 *    810223:  Added check for null data file list.
	 *===================================================================== */

} /* end of function */

