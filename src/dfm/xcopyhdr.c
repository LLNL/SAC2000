#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/lhf.h"
void /*FUNCTION*/ xcopyhdr(nerr)
int *nerr;
{
	char ktemp1[MCPFN+1], ktemp2[9], ktemp3[2][9];
	int lfirst, lfound, ltemp;
	int ic1, ic2, icatcox, idflco, itemcop, itemcox, itemp = 0, j, 
	 j_, jdfl, jdfl_, jhdrco, jhdrco_, nhdrco, notusd, ntemp;
	float ftemp;


	/*=====================================================================
	 * PURPOSE: To parse and execute the action command COPYHDR.
	 *          This command copies header variables from one file to
	 *          remaining files in DFL.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCPW
	 *    DFM:     NDFL, MHDRCO
	 *    LHF:     NKHDR
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    DFM:     NHDRCO, IDFLCO, ICATCO, ITEMCO
	 *    HDR:     Potentially all.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    880413:  Was not reinitializing header list correctly.
	 *    870209:  Converted to an internal command.
	 *    840906:  Original XSC version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870209
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	lfirst = TRUE;
L_1000:
	if( lcmore( nerr ) ){

		/* -- "FROM name|n":  determine which file to copy from. */
		if( lckey( "FROM#$",7 ) ){
			if( lcirc( 1, cmdfm.ndfl, &idflco ) ){
				}
			else if( lcchar( MCPFN, ktemp1,MCPFN+1, &notusd ) ){
				ic1 = 0;
				ic2 = 0;
				for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
					jdfl_ = jdfl - 1;
					if(lnxtcl( kmdfm.kdfl,MAXCHARS, &ic1, &ic2 )){
					  if( memcmp(ktemp1,kmdfm.kdfl+ic1 - 1,ic2-ic1+1) == 0 ){
						idflco = jdfl;
						goto L_1200;
						}
				        }
					ic1 = ic2 + 1;
					}
				ictok( -1 );
				cfmt( "BAD FILE NAME:$",16 );
				cresp();
				}
			else{
				cfmt( "NEED A FILE NAME OR A NUMBER:$",31 );
				cresp();
				}
L_1200:
			;

			/* -- "hdrvar":  name of a header variable to copy. */
			}
		else if( lcchar( MCPW, ktemp2,9, &notusd ) ){
			hdrfld( ktemp2,9, &icatcox, &itemcox, &lfound );
			if( lfound ){
				if( lfirst ){
					nhdrco = 0;
					lfirst = FALSE;
					}
				if( nhdrco < MHDRCO ){
					nhdrco = nhdrco + 1;
					Icatco[nhdrco] = icatcox;
					Itemco[nhdrco] = itemcox;
					}
				else{
					cfmt( "TOO MANY HEADER VARIABLES:$",28 );
					cresp();
					}
				}
			else{
				cfmt( "ILLEGAL HEADER VARIABLE:$",26 );
				cresp();
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

	if( *nerr != 0 )
		goto L_8888;

	/* CHECKING PHASE: */

	/* - Test for a non-null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* EXECUTION PHASE: */

	/* - Loop on each header itemco in list. */

	for( jhdrco = 1; jhdrco <= nhdrco; jhdrco++ ){
		jhdrco_ = jhdrco - 1;

		/* -- Get master file's header from memory manager. */
		getfil( idflco, FALSE, &notusd, &notusd, &notusd, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Get header variable from master file. */
		if( Icatco[jhdrco] == cmlhf.icatf ){
			ftemp = Fhdr[Itemco[jhdrco]];
			}
		else if( Icatco[jhdrco] == cmlhf.icati ){
			itemcop = Ihdr[Itemco[jhdrco]];
			}
		else if( Icatco[jhdrco] == cmlhf.icatn ){
			ntemp = Nhdr[Itemco[jhdrco]];
			}
		else if( Icatco[jhdrco] == cmlhf.icatl ){
			ltemp = Lhdr[Itemco[jhdrco]];
			}
		else if( Icatco[jhdrco] == cmlhf.icatk ){
			for( j = 1; j <= Nkhdr[Itemco[jhdrco]]; j++ ){
				j_ = j - 1;
				strcpy( ktemp3[j_], kmhdr.khdr[Itemco[jhdrco] + j_ - 
				 1] );
				}
			}

		/* -- Copy this variable to all (other) files in DFL. */
		for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
			jdfl_ = jdfl - 1;
			getfil( jdfl, FALSE, &notusd, &notusd, &notusd, nerr );
			if( *nerr != 0 )
				goto L_8888;
			if( Icatco[jhdrco] == cmlhf.icatf ){
				Fhdr[Itemco[jhdrco]] = ftemp;
				}
			else if( Icatco[jhdrco] == cmlhf.icati ){
				Ihdr[Itemco[jhdrco]] = itemp;
				}
			else if( Icatco[jhdrco] == cmlhf.icatn ){
				Nhdr[Itemco[jhdrco]] = ntemp;
				}
			else if( Icatco[jhdrco] == cmlhf.icatl ){
				Lhdr[Itemco[jhdrco]] = ltemp;
				}
			else if( Icatco[jhdrco] == cmlhf.icatk ){
				for( j = 1; j <= Nkhdr[Itemco[jhdrco]]; j++ ){
					j_ = j - 1;
					strcpy( kmhdr.khdr[Itemco[jhdrco] + j_ - 1], ktemp3[j_]
					  );
					}
				}

			/* -- Return file to memory manager. */
			putfil( jdfl, nerr );
			if( *nerr != 0 )
				goto L_8888;
			}

		}

L_8888:
	return;

} /* end of function */

