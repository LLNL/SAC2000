#include <stdio.h>
#include <string.h>
#include "complex.h"
#include "proto.h"
#include "dfm.h"
#include "ssi.h"
#include "msg.h"

#include "cssStrucs.h"
#include "cssListStrucs.h"
#include "dblPublicDefs.h"
#include "cssListOps.h"
#include "smDataIO.h"


void xrollback (/* kinput ,*/ nerr )
/*char * kinput ;*/
int * nerr ;
{
    /* Declare variables    Note: this will be used if command parsing is needed.
    char koption[MCMSG+1] , * ptrKoption ; */

        /*=====================================================================
         * PURPOSE:  To execute the action command ROLLBACK.
         *           Files in SAC are replaced with files from SeisMgr 
         *=====================================================================
         * INPUT ARGUMENTS:
         *    KINPUT:  Character string containing options
         *             Note, this command is not parsed by the standard SAC
	 *             parsing mechanism.
         *=====================================================================
         * OUTPUT ARGUMENTS:
         *    NERR:    Error flag. Set to 0 if no error occurred.
         *=====================================================================
         * MODULE/LEVEL: SSI/2
         *=====================================================================
         * GLOBAL INPUT:
         *    SSI:     LFULL 
         *=====================================================================
         * MODIFICATION HISTORY:
	 *	980915:	Original version.
         *=====================================================================*/

    *nerr = 0 ;

    rollback ( allHeader , nerr ) ;

} /* end xrollback */




/* this is how command parsing will work if the command ever gets any options */

    /* Look for the option:  FULL *
    strcpy ( koption , kinput ) ;
    ptrKoption = strtok ( koption , " \t" ) ;

    while ( ptrKoption != NULL )
    {
	int tokenLen ;

	tokenLen = strlen ( ptrKoption ) ;

	* Make all the characters upper-case for the purpose of comparison *
	modcase ( TRUE , ptrKoption , tokenLen , ptrKoption ) ;

	* Compare current token to acceptable options.  If there's a match,
	   process it. *
	if ( strcmp ( ptrKoption , "FULL" ) == 0 )
	{
	    int lfull ;

	    * look at the next token to see if it says OFF, ON or neither 
		(return 0, 1, or 2 respectively ) *
	    lfull = OnOrOff ( ptrKoption ) ;
	    cmssi_lfullRoll = lfull ? allHeader : wfHeader ;

	    * if lfull is less than 2, move beyond the dumb token *
	    if ( lfull < 2 ) 
		ptrKoption = strtok ( NULL , " \t" ) ;

	} * end FULL *

	else {
	    *nerr = 1001 ;
	    setmsg ( "ERROR" , *nerr ) ;
	    apcmsg ( ptrKoption , strlen ( ptrKoption ) + 1 ) ;
	    outmsg () ;
	    return ;
	}


	* Prepare for the next pass through the loop. *
	ptrKoption = strtok ( NULL , " \t" ) ;
    } * end while ( ptrKoption ) */

