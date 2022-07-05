#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lcdfl(kdflist, kdflist_s, indfl)
char *kdflist;   int kdflist_s;
int *indfl;
{
	char kname[MCPFN+1];
	int lcdfl_v;
	int idx, n1, n2, ncname, nerr;



	/*=====================================================================
	 * PURPOSE: To parse a "data file list" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *     LCDFL: .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *     kdflist: Character list of SAC data file names found. [cl]
	 *     indfl:   Number of entries in KDFL. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom, ncom, kcom, itypcm, inumbr, ialpha, flnum
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     jcom
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     basenm, lcchar, putcl
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970129:  Add parameter (0) to cnvati.  0 means that if a string
	 *             of digits is too int, let it slide by.  maf 
	 *    911107:  Changed calling parameter names to avoid debugger problem.
	 *    880915:  Deleted check for list beginning with a number.
	 *    820423:  Adjustments due to new command parsing system.
	 *    800823:  Modified the form of the base name option.
	 *             Added 32 chracter tree names by using ZFILNM [PRIME].
	 *    810120:  Changed to output message retrieval from disk.
	 *    810208:  Changed from DEFDFL to logical function LCDFL.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	nerr = 0;

	for( idx = 0 ; idx < MCPFN ; idx++ )
	    kname[ idx ] = ' ' ;
	kname[ MCPFN ] = '\0' ;

	/* - Forms of names that are currently supported are:
	 *   - NAME   ... normal file name
	 *   - 'LONGTREENAME'   ...   Prime tree name
	 *   - BASE N   ...   N names of form BASE01, BASE02 ...
	 *   - BASE M N   ... N-M+1 names starting with BASExx where xx=M */

	/* - Assume the worst. */

	lcdfl_v = FALSE;

	/* - Initialize the character list, counter, and kname. */

	memset ( kdflist , ' ' , kdflist_s - 1 );
	*indfl = 0;
	memset ( kname , ' ' , MCPFN ) ;
	kname [ MCPFN ] = '\0';

	/* - Loop until tokens are exhausted. */

	while( cmcom.jcom <= cmcom.ncom ){	/* -- Loop until command is exhausted. */

	    /* -- Use LCCHAR to get next filename. */
	    if( lcchar( MCPFN, kname,MCPFN+1, &ncname ) ){
		/*  --- Generate file names from base name if next symbol a number. */
		cnvati( (char*)kmcom.kcom[cmcom.jcom - 1],9, &n1, 0, &nerr );
		if( cmcom.jcom <= cmcom.ncom && nerr == 0 ){
		    if( cmcom.jcom + 1 <= cmcom.ncom ){
			cnvati( (char*)kmcom.kcom[cmcom.jcom],9, &n2, 0, &nerr );
			if( nerr == 0 ){
			    cmcom.jcom = cmcom.jcom + 1;
			}
			else{
			    n2 = n1;
			    n1 = 1;
			}
		    }
		    else{
			n2 = n1;
			n1 = 1;
		    }
		    basenm( kname,MCPFN+1, n1, n2, kdflist,kdflist_s, &nerr );
		    if( nerr != 0 )
			goto L_8888;
		    lcdfl_v = TRUE;
		    *indfl = *indfl + (n2 - n1 + 1);
		    cmcom.jcom = cmcom.jcom + 1;
		}

		/* --- Otherwise this is a simple filename. */
		else{
		    putcl( kdflist,kdflist_s, kname,MCPFN+1, &nerr );
		    if( nerr != 0 )
			goto L_8888;
		    lcdfl_v = TRUE;
		    *indfl = *indfl + 1;
		}
	    }

	    /* -- Raise error condition due to an unexpected token. */
	    else{
			nerr = 1001;
			goto L_8888;
	    }
	} /* end while */

L_8888:
	return( lcdfl_v );

} /* end of function */

