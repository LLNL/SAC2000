#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
#include "../../inc/lhf.h"
void /*FUNCTION*/ formhv(kname, kname_s, iform, kout, kout_s, nerr)
char *kname;   int kname_s;
int iform;
char *kout;   int kout_s;
int *nerr;
{
	char kvalue[41];
	int lok, lok2 = FALSE, linc ;  /* lok2 and linc for INC option in lh. maf 961212 */
	int icat, item, j, j1, j2, j_, jc, nc;
        int increment;
	int idx ;


	/*=====================================================================
	 * PURPOSE:  To format a header value into a text string.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Name of header field. [c]
	 *    IFORM:   Type of output format desired. [i]
	 *             = 1 gives name followed by " = " followed by value.
	 *             = 2 gives name followed by ": " followed by value.
	 *             = 3 gives value only.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KOUT:    Formatted output string. [c]
	 *    NERR:    Error return flag.  Set to 0 if no error occurred. [i]
	 *             = -1 if KNAME is undefined in current header.
	 *                  KOUT is still formatted in this case.
	 *             = -2 if bad value for IFORM.
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     FUNDEF, NUNDEF, IUNDEF, KUNDEF,
	 *             FHDR, NHDR, IHDR, KDIV, LHDR, KHDR
	 *    LHF:     ICATF, ICATN, ICATI, ICATL, ICATK, ICATA
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *     SACLIB: HDRFLD, LGAHDR, INDEXB
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    LOK:     Flag saying whether header field can be formatted. [l]
	 *    ICAT:    Indicates which type of header field was requested. [i]
	 *    ITEM:    Location in header array of requested field. [i]
	 *    KVALUE:  Used in formatting of header value. [c40]
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
        for( idx = 0 ; idx < 40 ; idx++ )
	   kvalue[ idx ] = ' ' ;
	kvalue[ 40 ] = '\0' ;

	/* if cmhdr.linc and .llh are both TRUE, so is linc. maf 961212 */
	linc = cmhdr.linc && cmhdr.llh ;

	/* - Determine type and location of header field. */

	hdrfld( kname,kname_s, &icat, &item, &lok );

	if( lok ){
		lok2 = TRUE ;	/* lok was true coming out of hdrfld().  maf 961212 */
		if( icat == cmlhf.icatf ){
			lok = Fhdr[item] != cmhdr.fundef ;
			if( lok ||  linc ){	/* added linc. maf 961212 */
						/* 16.5 became 16.6.  maf 970128 */
                                sprintf(kvalue,"%#16.6e",Fhdr[item]); 
				ljust( kvalue,41 );
			}
		}
		else if( icat == cmlhf.icatn ){
			lok = Nhdr[item] != cmhdr.nundef ;
			if( lok ||  linc  ){	/* added linc. maf 961212 */
                                sprintf(kvalue,"%10d", Nhdr[item]);
				ljust( kvalue,41 );
			}
		}
		else if( icat == cmlhf.icati ){
			lok = Ihdr[item] != cmhdr.iundef ;
			if( lok )
				fstrncpy( kvalue, 40, kmlhf.kdiv[Ihdr[item] - 1],
                                               strlen(kmlhf.kdiv[Ihdr[item] - 1]));
			else if ( linc )	/* added.  maf 961212 */
				strcpy( kvalue, "UNDEFINED                               " );
		} /* end else if( icat == cmlhf.icati ) */
		else if( icat == cmlhf.icatl ){
			lok = TRUE;
			if( Lhdr[item] ){
				strcpy( kvalue, "TRUE                                    " );
			}
			else{
				strcpy( kvalue, "FALSE                                   " );
			}
		}
		else if( icat == cmlhf.icatk ){
			lok = memcmp(kmhdr.khdr[item - 1],kmhdr.kundef,min(strlen(kmhdr.khdr[item - 1]),
                                  strlen(kmhdr.kundef))) != 0 ;
			if( lok ||  linc  ){	/* added linc. maf 961212 */
				j1 = item;
				j2 = j1 + Nkhdr[item] - 1;
				jc = 1;
                                increment = MCPW;
                                if(Nkhdr[item] > 1)increment = MCPW + 1;
				for( j = j1; j <= j2; j++ ){
					j_ = j - 1;
					subscpy( kvalue, jc - 1, jc + increment - 2, 40, kmhdr.khdr[j_]
					  );
					jc = jc + increment;
				}
				subscpy( kvalue, jc - 1, -1, 40, " " );
			}
		} /* end if( icat == cmlhf.icatk ) */
		else if( icat == cmlhf.icata ){
			lok = lgahdr( kname,kname_s, kvalue,41 );
		}
	} /* end if ( lok ) */

	if( !lok2 || ( !lok && !linc ) ) {	/* condition changed. maf 961212 */
		strcpy( kvalue, "Undefined                               " );
		*nerr = -1;
	}

	if( iform == 1 ){
		nc = indexb( kname,kname_s );
                fstrncpy(kout, kout_s-1, kname, nc);
                fstrncpy(kout+nc, kout_s-1-nc, " = ", 3);
                fstrncpy(kout+nc+3, kout_s-1-nc-3, kvalue, strlen(kvalue));
	}
	else if( iform == 2 ){
		nc = indexb( kname,kname_s );
                fstrncpy(kout, kout_s-1, kname, nc);
                fstrncpy(kout+nc, kout_s-1-nc, ": ", 2);
                fstrncpy(kout+nc+2, kout_s-1-nc-2, kvalue, strlen(kvalue));
	}
	else if( iform == 3 ){
                fstrncpy(kout, kout_s-1, kvalue, strlen(kvalue));
	}
	else{
		*nerr = -2;
		strcpy( kvalue, "Bad format number                       " );
	}

L_8888:

	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    970128:  Now allows floats 7 digits. maf
	 *    961212:  Modified to add INCLUSIVE option to the lh command.  maf
	 *    841025:  Original version.
	 *===================================================================== */

} /* end of function */

