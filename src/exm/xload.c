#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/comlists.h"
void /*FUNCTION*/ xload(nerr)
int *nerr;
{
	char kabbrev[9], kcommand[9], kdirpart[MCPFN+1], kfile[MCPFN+1], kfilepart[MCPFN+1], 
	 kname[MCPFN+1];
	int index, nc;



	/*=====================================================================
	 * PURPOSE: To parse the parameter-setting command LOAD. This command
	 *          dynamically loads an external SAC command into memory.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred. [i]
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  exm/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:      MCPFN, MCMSG, MCPW
	 *    comlists:  MEXTCOMNAMES
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    comlists:  nextcomnames, kextcomnames, iextcomindex
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lcmore, lcchar, lkchar, cfmt, cresp,
	 *             zsearchextcom, zloadextcom
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    kname:    Name of requested external command. [c]
	 *    kfile:    Expanded name of external command object file. [c]
	 *    kcommand: Name of external command. [k]
	 *    kabbrev:  Abbreviation of external command. [k]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900804:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900804
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/*   PARSING PHASE: */

	index = 0;
L_1000:
	if( lcmore( nerr ) ){

		/* -- ABBREV text: abbreviation for last external command. */
		if( lkchar( "ABBREV#$",9, MCPW, kabbrev,9, &nc ) ){
			modcase( TRUE, kabbrev, MCPW, kabbrev );
			if( index != 0 && cmcomlists.nextcomnames <= MEXTCOMNAMES ){
				cmcomlists.nextcomnames = cmcomlists.nextcomnames + 
				 1;
				strcpy( kmcomlists.kextcomnames[cmcomlists.nextcomnames - 1], kabbrev
				  );
				Iextcomindex[cmcomlists.nextcomnames] = index;
				}
			else{
				*nerr = 1027;
				setmsg( "ERROR", *nerr );
				apimsg( MEXTCOMNAMES );
				goto L_8888;
				}

			/* -- name: name of external command to load. May include directory path. */
			}
		else if( lcchar( MCPFN, kname,MCPFN+1, &nc ) ){
			zload( kname, &index, nerr );
			if( *nerr != 0 )
				goto L_8888;
			getdir( kname,MCPFN+1, kdirpart,MCPFN+1, kfilepart,MCPFN+1 );
			modcase( TRUE, kfilepart, MCPW, kcommand );
			if( cmcomlists.nextcomnames <= MEXTCOMNAMES ){
				cmcomlists.nextcomnames = cmcomlists.nextcomnames + 
				 1;
				strcpy( kmcomlists.kextcomnames[cmcomlists.nextcomnames - 1], kcommand
				  );
				Iextcomindex[cmcomlists.nextcomnames] = index;
				}
			else{
				*nerr = 1027;
				setmsg( "ERROR", *nerr );
				apimsg( MEXTCOMNAMES );
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

