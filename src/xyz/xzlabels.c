#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/contouring.h"
void /*FUNCTION*/ xzlabels(nerr)
int *nerr;
{
	char klist[MCMSG+1];
	int llabelmode;
	int ic, ic1, ic2, itype, j, nc, nclist, nspacing;
	float angle, size, spacing[3];
        char *s1;


	float *const Spacing = &spacing[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To execute the parameter-setting command ZLABELS.
	 *           This command controls contour linestyle labeling for
	 *           subsequent contour plots.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *   NERR:   0 - no error, .ne. 0 - error
	 *=====================================================================
	 * MODULE/LEVEL:  xyz/2
	 *=====================================================================
	 * GLOBAL INPUT: 
	 *   mach:        MCMSG, TORAD
	 *   contouring:  MZLEVELS
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *   contouring:  klabelmode, klabellist, nlabellist, desiredangle,
	 *                minlabelspacing, deslabelspacing, maxlabelspacing
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:  lcmore, lclog, lkra, lkrrc, lkrest
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    901204:  Added font option for contour labels.
	 *    900425:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900425
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Parse positional tokens here. */

	/* -- "ON|OFF":  turn line labeling on or off. */
	if( lclog( &llabelmode ) ){
		if( llabelmode ){
			strcpy( kmcontouring.klabelmode, "ON      " );
			}
		else{
			strcpy( kmcontouring.klabelmode, "OFF     " );
			}
		}

	/* - Loop on remaining tokens in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "SPACING v1 v2 v3:  set line label spacing attributes. */
		if( lkra( "SPACING$",9, 1, 3, spacing, &nspacing ) ){
			if( nspacing == 1 ){
				cmcontouring.minlabelspacing = Spacing[1];
				}
			else if( nspacing == 2 ){
				cmcontouring.minlabelspacing = Spacing[1];
				cmcontouring.deslabelspacing = Spacing[2];
				}
			else if( nspacing == 3 ){
				cmcontouring.minlabelspacing = Spacing[1];
				cmcontouring.deslabelspacing = Spacing[2];
				cmcontouring.maxlabelspacing = Spacing[3];
				}

			/* -- "SIZE v:  set line label spacing text size. */
			}
		else if( lkreal( "SI#ZE$",7, &size ) ){
			cmcontouring.widthlabels = size;
			cmcontouring.heightlabels = size;

			/* -- "ANGLE v":  set desired maximum angle for labels. */
			}
		else if( lkrrc( "ANGLE$",7, 0.0, 90.0, &angle ) ){
			cmcontouring.desiredangle = TORAD*angle;

			/* -- "LIST ON|OFF|format|text ...":  set list of line label attributes. */
			}
		else if( lkrest( "LIST$",6, MCMSG, klist,MCMSG+1, &nclist ) ){
			j = 0;
			ic = 0;
L_1100:
			poptok( klist, nclist, &ic, &ic1, &ic2, &itype );
			if( itype > 0 ){
				nc = ic2 - ic1 + 1;
				j = min( j + 1, MZLEVELS );
				fstrncpy( kmcontouring.klabellist[j - 1], 16, klist+ic1 - 
				 1,min(ic2,MCMSG) - ic1 + 1);

				if( strcmp(kmcontouring.klabellist[j - 1],"on              ") == 
				 0 ){
					strcpy( kmcontouring.klabellist[j - 1], "ON              " );
					}
				else if( strcmp(kmcontouring.klabellist[j - 1],"off             ") == 
				 0 ){
					strcpy( kmcontouring.klabellist[j - 1], "OFF             " );
					}
				else if( memcmp(kmcontouring.klabellist[j - 1],"int",3) == 0 ){
					strcpy( kmcontouring.klabellist[j - 1], "INT             " );
					}
				else if( memcmp(kmcontouring.klabellist[j - 1],"float",5) == 0 ){
					subscpy( kmcontouring.klabellist[j - 1], 0, 4, 16, "FLOAT"
					  );
					if( nc > 5 ){
                                                strncpy((s1=malloc(ic2-(ic1+5)+2)),klist+ic1 + 
						 4, ic2 - (ic1 + 5) + 1);
                                                s1[ic2 - (ic1 + 5) + 1] = '\0';
						subscpy( kmcontouring.klabellist[j - 1], 5, -1, 16, s1 );
                                                free(s1);
					      }
					}
				else if( memcmp(kmcontouring.klabellist[j - 1],"exp",3) == 0 ){
					subscpy( kmcontouring.klabellist[j - 1], 0, 2, 16, "EXP"
					  );
					if( nc > 3 ){
                                                strncpy((s1=malloc(ic2-(ic1+3)+2)),klist+ic1 + 
						 2, ic2 - (ic1 + 3) + 1);
                                                s1[ic2 - (ic1 + 3) + 1] = '\0';
						subscpy( kmcontouring.klabellist[j - 1], 3, -1, 16, s1 );
                                                free(s1);
					      }
					}
				goto L_1100;
				}
			cmcontouring.nlabellist = j;

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

