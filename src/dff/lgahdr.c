#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
int /*FUNCTION*/ lgahdr(kfield, kfield_s, kvalue, kvalue_s)
char *kfield;   int kfield_s;
char *kvalue;   int kvalue_s;
{
	char kcmpaz[9], kcmpin[9], ktemp[9];
	int lgahdr_v;
	int ic1, ic2, nc, nerr;
        char *cattemp;


	/*=====================================================================
	 * PURPOSE: To get an auxiliary header field from current file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KFIELD:  Auxiliary header field [k]
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    .TRUE.   Header field(s) defined.
	 *    .FALSE.  Header field(s) undefined or bad input.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KVALUE:  Value of auxiliary field [c]
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     IDFLC, KDFL
	 *    HDR:     NZYEAR, NZJDAY, NZHOUR, NZMIN, NZSEC, NZMSEC,
	 *             KSTNM, CMPAZ, CMPINC
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  KADATE, KATIME, CNVITA
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Convert the name to upper case before doing any tests. */
	nc = min( MCPW, (kfield_s - 1) );
	modcase( TRUE, kfield, nc, ktemp );


	/* - Compute the requested auxiliary header field from actual header
	 *   fields, making sure that each one is defined. */

	/* -- KZDATE:  Zero date field: */

	if( memcmp(ktemp,"KZDATE",6) == 0 ){
		if( *nzyear != cmhdr.nundef && *nzjday != cmhdr.nundef ){
			nc = (kvalue_s - 1);
			kadate( *nzyear, *nzjday, nc, kvalue,kvalue_s, &nerr );
			if( nerr == 0 ){
				lgahdr_v = TRUE;
				}
			else{
				fstrncpy( kvalue, kvalue_s-1, "BAD FIELD", 9);
				lgahdr_v = FALSE;
				}
			}
		else{
			fstrncpy( kvalue, kvalue_s-1, "UNDEFINED", 9);
			lgahdr_v = FALSE;
			}

		/* -- KZTIME:  Zero time field: */

		}
	else if( memcmp(ktemp,"KZTIME",6) == 0 ){
		if( ((*nzhour != cmhdr.nundef && *nzmin != cmhdr.nundef) && 
		 *nzsec != cmhdr.nundef) && *nzmsec != cmhdr.nundef ){
			nc = (kvalue_s - 1);
			katime( *nzhour, *nzmin, *nzsec, *nzmsec, nc, kvalue,kvalue_s, 
			 &nerr );
			if( nerr == 0 ){
				lgahdr_v = TRUE;
				}
			else{
				fstrncpy( kvalue, kvalue_s-1, "BAD FIELD", 9);
				lgahdr_v = FALSE;
				}
			}
		else{
			fstrncpy( kvalue, kvalue_s-1, "UNDEFINED", 9);
			lgahdr_v = FALSE;
			}

		/* -- KSTCMP:  Station component field: */

		}
	else if( memcmp(ktemp,"KSTCMP",6) == 0 ){
		if( memcmp(kstnm,kmhdr.kundef,9) != 0 ){
			lgahdr_v = TRUE;
			nc = indexb( kstnm,9 );
			fstrncpy( kvalue, kvalue_s-1, kstnm , nc );
			if( memcmp(kcmpnm,kmhdr.kundef,9) != 0 ){
                                cattemp = malloc(2+strlen(kcmpnm)+1);
                                strcpy(cattemp,"  ");
                                strcat(cattemp,kcmpnm);
				subscpy( kvalue, nc, -1, kvalue_s - 1, cattemp );
                                free(cattemp);
				}
			else if( *cmpaz != cmhdr.fundef && *cmpinc != cmhdr.fundef ){
				if( *cmpaz == 0. && *cmpinc == 0. ){
					subscpy( kvalue, nc, -1, kvalue_s - 1, "  VERT"
					  );
					}
				else if( *cmpaz == 0 && *cmpinc == 90. ){
					subscpy( kvalue, nc, -1, kvalue_s - 1, "  NORTH"
					  );
					}
				else if( *cmpaz == 90. && *cmpinc == 90. ){
					subscpy( kvalue, nc, -1, kvalue_s - 1, "  EAST"
					  );
					}
				else{
					cnvita( (int)( *cmpaz + 0.5 ), kcmpaz,9 );
					ljust( kcmpaz,9 );
                                        cattemp = malloc(2+3+1);
                                        strcpy(cattemp,"  ");
                                        strncat(cattemp,kcmpaz,3);
					subscpy( kvalue, nc, nc + 4, kvalue_s - 1, cattemp );
                                        free(cattemp);
					nc = nc + 5;
					if( *cmpinc != 90. ){
						cnvita( (int)( *cmpinc + 0.5 ), kcmpin,9 );
						ljust( kcmpin,9 );
                                                cattemp = malloc(2+2+1);
                                                strcpy(cattemp,"  ");
                                                strncat(cattemp,kcmpin,2);
						subscpy( kvalue, nc, nc + 3, kvalue_s - 
						                            1, cattemp );
                                                free(cattemp);
						nc = nc + 4;
						}
					}
				}
			}
		else{
			fstrncpy( kvalue, kvalue_s-1, "UNDEFINED", 9);
			lgahdr_v = FALSE;
			}

		/* -- FILENAME:  Name of data file field: */

		}
	else if( memcmp(ktemp,"FILENAME",8) == 0 || memcmp(ktemp,"NAME",4) == 0 ){
		if(lnumcl( kmdfm.kdfl,MAXCHARS, cmdfm.idflc, &ic1, &ic2 )) {
		  fstrncpy( kvalue, kvalue_s-1, kmdfm.kdfl+ic1 - 1, ic2 - ic1 + 1);
	        }else{
                  fstrncpy(kvalue,kvalue_s-1," ",1);
	        }
		  lgahdr_v = TRUE;

		/* -- XMARKER:  X time pick and KX descriptor where
		 *              (where X is A, O, F, T0, T1, ... T9) */

		}
	else if( memcmp(ktemp,"AM",2) == 0 ){
		formmarker( *a, ka,9, kvalue,kvalue_s, &lgahdr_v );
		}
	else if( memcmp(ktemp,"OM",2) == 0 ){
		formmarker( *o, ko,9, kvalue,kvalue_s, &lgahdr_v );
		}
	else if( memcmp(ktemp,"FM",2) == 0 ){
		formmarker( *f, kf,9, kvalue,kvalue_s, &lgahdr_v );
		}
	else if( memcmp(ktemp,"T0M",3) == 0 ){
		formmarker( *t0, kt0,9, kvalue,kvalue_s, &lgahdr_v );
		}
	else if( memcmp(ktemp,"T1M",3) == 0 ){
		formmarker( *t1, kt1,9, kvalue,kvalue_s, &lgahdr_v );
		}
	else if( memcmp(ktemp,"T2M",3) == 0 ){
		formmarker( *t2, kt2,9, kvalue,kvalue_s, &lgahdr_v );
		}
	else if( memcmp(ktemp,"T3M",3) == 0 ){
		formmarker( *t3, kt3,9, kvalue,kvalue_s, &lgahdr_v );
		}
	else if( memcmp(ktemp,"T4M",3) == 0 ){
		formmarker( *t4, kt4,9, kvalue,kvalue_s, &lgahdr_v );
		}
	else if( memcmp(ktemp,"T5M",3) == 0 ){
		formmarker( *t5, kt5,9, kvalue,kvalue_s, &lgahdr_v );
		}
	else if( memcmp(ktemp,"T6M",3) == 0 ){
		formmarker( *t6, kt6,9, kvalue,kvalue_s, &lgahdr_v );
		}
	else if( memcmp(ktemp,"T7M",3) == 0 ){
		formmarker( *t7, kt7,9, kvalue,kvalue_s, &lgahdr_v );
		}
	else if( memcmp(ktemp,"T8M",3) == 0 ){
		formmarker( *t8, kt8,9, kvalue,kvalue_s, &lgahdr_v );
		}
	else if( memcmp(ktemp,"T9M",3) == 0 ){
		formmarker( *t9, kt9,9, kvalue,kvalue_s, &lgahdr_v );

		/* -- Invalid field: */

		}
	else{
		fstrncpy( kvalue, kvalue_s-1, "INVALID FIELD", 13);
		lgahdr_v = FALSE;

		}

L_8888:

	return( lgahdr_v );

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    900419:  Fixed bug in computing length of kfield.
	 *    880419:  Converted input field to uppercase before comparison.
	 *    850516:  Allowed NAME as an alias for FILENAME.
	 *    841026:  Added FILENAME field.
	 *    801101:  Added logic to treat an undefined station name.
	 *    801018:  Original version.
	 *===================================================================== */

} /* end of function */

