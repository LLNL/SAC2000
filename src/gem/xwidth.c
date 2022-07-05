#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ xwidth(nerr)
int *nerr;
{
	char ktok[9];
	int lnum;
	int inum;
	float rnum;


	/*=====================================================================
	 * PURPOSE: To parse the parameter-setting command WIDTH.
	 *          WIDTH controls the line-width display attributes.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    GEM:     MWIDTH
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     LWIDTH, LIWIDTH, IWIDTH, IIWIDTH, ISKWIDTH,
	 *             NIWIDTH, JIWIDTH
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCLOG, LCTOK, ICTOK
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *  920526: Original version - implemented from COLOR command.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Parse position-dependent tokens: */

	/* -- Turning WIDTH ON/OFF toggles current widths with the thin width. */
	if( lclog( &cmgem.lwidth ) ){
		if( cmgem.lwidth ){
			cmgem.iwidth = cmgem.iswidth;
			cmgem.iskwidth = cmgem.isskwidth;
			}
		else{
			cmgem.lwidth = TRUE;
			setlinewidth( cmgem.ithin );
			cmgem.lwidth = FALSE;
			cmgem.iswidth = cmgem.iwidth;
			cmgem.isskwidth = cmgem.iskwidth;
			}

		}
	else if( lctok( ktok,9, &lnum, &rnum ) ){
		inum = (int)( rnum + 0.1 );
		if( lnum ){
			cmgem.iwidth = inum;
			cmgem.lwidth = TRUE;
			ictok( 1 );
			}
		}

	/* - Parse position-independent tokens: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "SKELETON width":  change skeleton width. */
		if( lckey( "SK$",4 ) || lckey( "BO$",4 ) ){
			if( lctok( ktok,9, &lnum, &rnum ) )
				inum = (int)( rnum + 0.1 );
			if( lnum ){
				cmgem.iskwidth = inum;
				cmgem.lwidth = TRUE;
				ictok( 1 );
				}
			else{
				cfmt( "NEED SKELETON WIDTH VALUE:$",28 );
				cresp();
				ictok( -1 );
				}

			/* -- "LIST STANDARD/widthlist":  change the width list. */
			}
		else if( lckey( "L$",3 ) ){
			if( lckey( "S$",3 ) ){
				iniwidth();
				}
			else{
				cmgem.niwidth = 0;
L_1100:
				if( lctok( ktok,9, &lnum, &rnum ) ){
					inum = (int)( rnum + 0.1 );
					if( lnum ){
						if( cmgem.niwidth < MWIDTH )
							cmgem.niwidth = cmgem.niwidth + 1;
						Iiwidth[cmgem.niwidth] = inum;
						ictok( 1 );
						goto L_1100;
						}
					}
				if( cmgem.niwidth <= 0 )
					iniwidth();
				cmgem.iwidth = Iiwidth[1];
				cmgem.lwidth = TRUE;
				cmgem.jiwidth = 0;
				}

			/* -- "INCREMENT ON/OFF":  increment width after each file or not. */
			}
		else if( lklog( "I$",3, &cmgem.liwidth ) ){
			cmgem.lwidth = TRUE;
			cmgem.jiwidth = 0;

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

