#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gem.h"
#include "../../inc/hdr.h"
#include "../../inc/gam.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ xgt(nerr)
int *nerr;
{
	int lhardw;
	int index;



	/*=====================================================================
	 * PURPOSE:  To execute the parameter-setting command GTEXT.
	 *           This command defines certain graphic text attributes.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL: gem/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gem:     kgtqua, igtfnt
	 *             tsdef, tsaxis, tstitl, tsxlab, tsylab, tsplab
	 *    gam:     tsfid, tspk
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - All text size attributes are initialized by INIGEM, INIGAM, etc.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:    lcmore, cfmt, cresp, lkint, lklist
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900312:  Fixed bug in setting text type.
	 *    841102:  Changed names of several options.
	 *    840621:  Added ability to set text height/width ratio.
	 *    820817:  Changed to newest set of parsing and checking functions.
	 *    820325:  Added option to set default character size.
	 *    811013:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900312
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "HARDWARE|SOFTWARE": define new text quality. */
		if( lclog2( "HARDWARE$",10, "SOFTWARE$",10, &lhardw ) ){
			if( lhardw ){
				strcpy( kmgem.kgtqua, "HARDWARE" );
				}
			else{
				strcpy( kmgem.kgtqua, "SOFTWARE" );
				}
			settexttype( kmgem.kgtqua );

			/* -- "FONT n":  define new software text font. */
			}
		else if( lkint( "FONT$",6, &cmgem.igtfnt ) ){

			/* -- "FORCE": Force Hardware fonts */
			}
		else if( lckey( "FOR#CE$",8 ) ){
			cmgdm.lfhard = TRUE;
			strcpy( kmgem.kgtqua, "HARDWARE" );
			settexttype( kmgem.kgtqua );

			/* -- "SIZE TINY|SMALL|MEDIUM|LARGE":  set new default char. size. */
			}
		else if( lklist( "SI#ZE$",7, (char*)kmgem.ktxsiz,9, MTXSIZ, 
		 &index ) ){
			cmgem.tsdef = Txsiz[index];
			cmgem.tsaxis = cmgem.tsdef;
			cmgem.tstitl = cmgem.tsdef;
			cmgem.tsxlab = cmgem.tsdef;
			cmgem.tsylab = cmgem.tsdef;
			cmgam.tsfid = cmgem.tsdef;
			cmgam.tspk = cmgem.tsdef;
			cmgem.tsplab[0] = cmgem.tsdef;

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

