#include <stdio.h>
#include <string.h>
#include "complex.h"
#include "proto.h"
#include "ssi.h"
#include "msg.h"


void xcommit ( /* kinput ,*/ nerr )
/*char * kinput ;*/
int * nerr ;
{
    /* Declare variables     Note: this will be used if command parsing is needed. 
    char koption[MCMSG+1] , * ptrKoption ; */

        /*=====================================================================
         * PURPOSE:  To execute the action command COMMIT.
         *           Files in SAC are copied to SeisMgr 
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
         *    SSI:     LDATA
         *=====================================================================
         * MODIFICATION HISTORY:
	 *	980915:	Original version.
         *=====================================================================*/

    *nerr = 0 ;

    sacToSeisMgr ( 0 , 0 , 1 , nerr ) ;
} /* end commit */




/* the following is how command parsing would work if commit ever gets any options. */

    /* Look for the option:  DATA *
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
	if ( strcmp ( ptrKoption , "DATA" ) == 0 )
	{
	    int ldata ;

	    * look at the next token to see if it says OFF, ON or neither 
		(return 0, 1, or 2 respectively ) *
	    ldata = OnOrOff ( ptrKoption ) ;
	    cmssi_ldataCommit = ldata ? TRUE : FALSE ;

	    * if ldata is less than 2, move beyond the dumb token *
	    if ( ldata < 2 ) 
		ptrKoption = strtok ( NULL , " \t" ) ;

	} * end DATA *

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




