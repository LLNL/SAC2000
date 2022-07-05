#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/eam.h"
void /*FUNCTION*/ xwhpf(nerr)
int *nerr;
{
	int notusd;


	/*=====================================================================
	 * PURPOSE: To parse and execute the action command WHPF.
	 *          This command writes auxiliary cards to the HYPO pick file (HPF).
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  EAM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:   
	 *    EAM:     LHPFOP, LHPFIC, NHPFIC()
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    EAM:     LHPFIC, NHPFIC()
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, GTOUTM, LKIA
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820810:  Changed names for HPF and APF variables.
	 *    820621:  Changed to newest set of parsing and checking functions.
	 *    810220:  Changed to output message retrieval from disk.
	 *    800308:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820624
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "IC n1 n2":  change IC constants. */
		if( lkia( "IC$",4, 1, 2, cmeam.nhpfic, &notusd ) ){
			cmeam.lhpfic = TRUE;

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

	if( *nerr != 0 )
		goto L_8888;

	/* EXECUTION PHASE: */

	/* - Make sure HPF is open. */

	if( !cmeam.lhpfop ){
		*nerr = 1908;
		setmsg( "ERROR", *nerr );
		goto L_8888;
		}

	/* - Write IC card to HPF if requested. */

	if( cmeam.lhpfic )
		{
                fprintf(cmeam.nhpfun,"                 %1d%1d\n", Nhpfic[1], 
		                                                    Nhpfic[2] );
		}

L_8888:
	return;

} /* end of function */

