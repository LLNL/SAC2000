#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MI	16
#define	MOK	10

#include "../../inc/mach.h"
#include "../../inc/cnv.h"
void /*FUNCTION*/ cnvati(kintgr, kintgr_s, intgr, lstrict, nerr)
char *kintgr;   int kintgr_s;
int *intgr, lstrict, *nerr;	/* lstrict added. maf 970129 */
{
	int ldone;
	byte kch;
	int ifac, isign, j1, j2, jch, nch, ni[MI];
        int nchp;
	static byte kok[MOK]={'0','1','2','3','4','5','6','7','8','9'};

	byte *const Kok = &kok[0] - 1;
	int *const Ni = &ni[0] - 1;
        char *pkintgr;

	/*=====================================================================
	 * PURPOSE: To convert an ascii symbol to its integer equivalent.
	 *          An error flag is set if the symbol is alphanumeric.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KINTGR:  ASCII integer.  Max of 16 characters int. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    INTGR:   Integer equivalent. [i]
	 *    NERR:    Error return flag.  Set to 0 if no error occurred.
	 *             Set to 1 if KINTGR is alphanumeric.
	 *             Set to 2 if KINTGR is too int (more than 16 characters.)
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *   MACH: MLARGE:  Max signed integer value.
	 *   CNV:  ICNVER:  Non-zero if conversion error occur [i].
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *   CNV:  ICNVER
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    KOK:     List of legal integers. [c1]
	 *    MOK:     Length of KOK. [i]
	 *    KCH:     Current character being parsed. [c1]
	 *    JCH:     Current character index in KINTGR. [i]
	 *    NI:      Array used to store digits during parsing. [i]
	 *    MI:      Length of NI.  This determines max length of KINTGR. [i]
	 *    ISIGN:   Sign of integer. [i]
	 *    J1,J2:   Loop indices and counters. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *    970129:  Added lstrict to indicate how to deal with errors of
         *             a string of digits that is to int to be converted to
         *             an int.  1 means catch the error and warn the user;
         *             0 means let it slide unnoticed.  maf 
	 *    970102:  Fixed bug reading large integers by removing the 
	 *             variable: linterr.  Also cleaned up some of the comments,
	 *             indenting, and goto's. 
	 *    920326:  Added WARNING msg if integer value is possibly
	 *             too large. Added ../../inc/mach for max inter value.
	 *             Added ../inc/cnv to store conversion error number.
	 *    820304:  Added variable length symbol capability.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

        /* - Remove leading zeros */
        nchp = kintgr_s;
        pkintgr = kintgr;
        while( (*pkintgr == '0') && (nchp > 0)){
          pkintgr++;
          nchp--;
	}

	/* - Determine length of input string. */

	nch = indexb( pkintgr, nchp);

        if ( nch == 0 ){
	    *intgr = 0;
            goto L_8888;
	}
	/* - Make sure input string is not too int. */


        if ( nch == 0 ){
	    *intgr = 0;
            goto L_8888;
	}


	if( nch > MI ){
		*nerr = 2;
		goto L_8888;
	}

     	/* - Uses Fortran 77 character substrings to scan input symbol.
	 * - Scan is stopped when NCH characters have been evaluated or
	 *   when an illegal or blank character is encountered. */

	jch = 0;
	j1 = 0;
	isign = 1;
	ldone = FALSE;

	do {
	    jch++ ;
	    kch = pkintgr[jch - 1];

	    /* -- Check for sign of integer. */
	    if( kch == '+' ){
		if( jch == 1 ){
			isign = 1;
		}
		else{
			*nerr = 1;
			goto L_8888;
		}
	    }
	    else if( kch == '-' ){
		if( jch == 1 ){
			isign = -1;
		}
		else{
			*nerr = 1;
			goto L_8888;
		}
	    }

	    /* -- A blank terminates conversion. */
	    else if( kch == ' ' )
		ldone = TRUE;

	    /* -- Check for integers if ok.  Save each digit.
	     *    Error if an illegal character is found. */
	    else{
		j2 = 1;
L_4000:
		if( kch != Kok[j2] ){
			if( j2 < MOK ){
				j2 = j2 + 1;
				goto L_4000;
			}
			else{
				*nerr = 1;
				goto L_8888;
			}
		}
		else{
			j1 = j1 + 1;
			Ni[j1] = j2 - 1;
		}
	    }

	}while( jch < nch && !ldone ) ; /* Loop if more chars and we're not done. */

	/* - Build integer from the stored digits. */
	ifac = 1;
	*intgr = 0;
	for( j2 = j1; j2 >= 1; j2-- ){
		/* -- Warning.. Approaching maximum integer size.
		 * - Use a fudge factor of 100k to test present integer value.*/
		if( (ifac == 1000000000) || (*intgr >= (MLARGE - 100000)) ){
		    if ( lstrict ) {	/* lstrict added. maf 970129 */
			cmicnv.icnver = 4003;
			setmsg( "WARNING", cmicnv.icnver );
			apcmsg( "integer too large\a", 19 );
			outmsg();
			clrmsg();
		    }
		}
		else{
			*intgr = *intgr + Ni[j2]*ifac;
		}
		ifac = 10*ifac;
	}
	*intgr = isign**intgr;

L_8888:
	return;
} /* end of function */

