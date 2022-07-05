#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/exm.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ xrerr(nerr)
int *nerr;
{
	int index;



	/*=====================================================================
	 * PURPOSE:  To execute the parameter-setting command READERR.
	 *           This command defines actions to be taken when certain
	 *           errors occur during the execution of the READ command.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL: DFM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    EXM:     KECTP(), NECTP
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    EXM:     KECNOF
	 *    DFM:     KECBDF, KECMEM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LKLIST
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "BADFILE FATAL|WARNING|IGNORE":  error control for missing
		 *                                     or unreadable data file. */
		if( lklist( "BADFILE$",9, (char*)kmexm.kectp,9, cmexm.nectp, 
		 &index ) ){
			strcpy( kmdfm.kecbdf, kmexm.kectp[index - 1] );

			/* -- "NOFILES FATAL|WARNING|IGNORE":  error control for null data file list. */
			}
		else if( lklist( "NOFILES$",9, (char*)kmexm.kectp,9, cmexm.nectp, 
		 &index ) ){
			strcpy( kmexm.kecnof, kmexm.kectp[index - 1] );

			/* -- "MEMORY SAVE|DELETE":  error control for memory contents. */
			}
		else if( lklist( "MEMORY$",8, (char*)kmexm.kectp,9, cmexm.nectp, 
		 &index ) ){
			strcpy( kmdfm.kecmem, kmexm.kectp[index - 1] );

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

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    920501:  Added KECMEM, save or delete contents in memory.
	 *    820817:  Changed to newest set of parsing and checking functions.
	 *    820113:  Merged old ERRCON command into this command.
	 *===================================================================== */

} /* end of function */

