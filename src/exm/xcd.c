#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/comlists.h"
void /*FUNCTION*/ xcd(nerr)
int *nerr;
{
	char kabbrev[9], kcommand[9], kdirpart[MCPFN+1], kfile[MCPFN+1], kfilepart[MCPFN+1], 
	 kname[MCPFN+1];
	int index, nc;



	/*=====================================================================
	 * PURPOSE:  To change SAC's current working directory. 
	 *   
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred. [i]
	 *  
	 *=====================================================================
	 * MODULE/LEVEL:  exm/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:      MCPFN, MCMSG, MCPW
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lcmore, lcchar, lkchar, cfmt, cresp,
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    kname:    Name of requested external command. [c]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    102795:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/*   PARSING PHASE: */

	index = 0;
L_1000:
	if( lcmore( nerr ) ){

         /* Get the name of a directory to change to. */
		if( lcchar( MCPFN, kname,MCPFN+1, &nc ) ){
                  kname[nc] = '\0';
                  if( chdir(kname) != 0 ){
                    *nerr = 124;
                    goto L_8888;
                  }  

			/* -- Bad syntax. */
		}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;

		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

} /* end of function */

