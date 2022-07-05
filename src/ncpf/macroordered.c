#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/cpf.h"
void /*FUNCTION*/ macroordered(kmacroargs, kmacroargs_s, nun, kline, 
	 kline_s, nerr)
char *kmacroargs;   int kmacroargs_s;
FILE *nun;
char *kline;   int kline_s;
int *nerr;
{
	char _c0[2], kdef[MCMSG+1], key[MCMSG+1], kmacroname[MCPFN+1], ktoken[9], 
	 kval[MCMSG+1];
	int iatype, ic1, ic2, ica1, ica2, itype, jcargs, jline, nc, 
	 ncargs, ndef, nkey, nline, nval, numsave;
        char *s1;

	/*=====================================================================
	 * PURPOSE: To process an "ordered" SAC macro file preamble.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kmacroargs:  Arguments from macro execute line. [c]
	 *    nun:         Fortran file unit that macro file is open on. [i]
	 *    kline:       Next line from command file. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag.  Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG, MCPW, MCPFN
	 *    cpf:     knoval, kvarsname
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900322:  Fixed bug so that quoted strings are treated as
	 *             a single argument and pass through accordingly.
	 *    870915:  Added ability to skip over blank or comment lines.
	 *    870402:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870402
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Store the entire argument line in the vars section under the keyword "all". */

	ncargs = indexb( kmacroargs,kmacroargs_s );
	if( ncargs > 0 )
		putvvstring( kmcpf.kvarsname,9, "all",4, ncargs, kmacroargs
		 ,kmacroargs_s, nerr );

	/* - Process argument line.  Each token is stored in numerical order.
	 *   The jth token is stored as the keyword "j". */

	jcargs = 0;
	nkey = 0;
L_1000:
	poptok( kmacroargs, ncargs, &jcargs, &ica1, &ica2, &iatype );
	if( iatype > 0 ){
		nkey = nkey + 1;
                sprintf(key,"%8d",nkey);
		ljust( key,MCMSG+1 );
		nval = ica2 - ica1 + 1;
		if( iatype == 3 ){
                        fstrncpy( kval, MCMSG, kmacroargs+ica1 - 
			 1,ica2 - ica1 + 1);
                        *(kval+(ica2-ica1+1)) = kmacroargs[ica1 - 1];
			}
		else{
			fstrncpy( kval, MCMSG, kmacroargs+ica1 - 1,ica2 - ica1 + 
			 1);
			}
		putvvstring( kmcpf.kvarsname,9, key,MCMSG+1, nval, kval,MCMSG+1, nerr );
		if( *nerr != 0 )
			goto L_8888;
		goto L_1000;
		}

	/* - For each default card in macro preamble, starting with the one
	 *   passed in by the calling subroutine:
	 *   (Format of a default card is: $DEFAULT keyword value)
	 *   If keyword already has a definition do nothing.
	 *   Otherwise store the default value.
	 * - If not a default card, return. */

	nline = indexb( kline,kline_s );
L_2000:
	jline = 0;
        ic1 = 0;
        ic2 = 0;
	poptok( kline, nline, &jline, &ic1, &ic2, &itype );

        if( ic2 >= ic1 ) {
          strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2 - ic1 + 1);
          s1[ic2 - ic1 + 1] = '\0';
	  upcase( s1, ic2 - ic1 + 1, ktoken,9 );
          free(s1);
	  if( strcmp(ktoken,"$DEFAULT") == 0 ){
		poptok( kline, nline, &jline, &ic1, &ic2, &itype );
		ndef = ic2 - ic1 + 1;
		fstrncpy( kdef, MCMSG, kline+ic1 - 1,ic2 - ic1 + 1);
		nval = nline - jline + 1;
		fstrncpy( kval, MCMSG, kline+jline - 1,nline - jline + 1);
		if( !existsv( kmcpf.kvarsname,9, kdef,MCMSG+1 ) ){
			putvvstring( kmcpf.kvarsname,9, kdef,MCMSG+1, nval, kval,MCMSG+1, 
			 nerr );
			if( *nerr != 0 )
				goto L_8888;
			}
L_3000:
                if(fgets( kline,kline_s,nun)==NULL){
                  if(feof(nun))goto L_8888;
                  goto L_9000;
	        }
                if(kline[(numsave=strlen(kline)-1)] == '\n') kline[numsave] = ' ';

		nline = indexb( kline,kline_s );
		if( nline <= 0 || kline[0] == '*' )
			goto L_3000;
		goto L_2000;
	      }
      }
L_8888:

	return;

L_9000:
	*nerr = 114;
	setmsg( "ERROR", *nerr );
	apcmsg( "macro ordered preamble for",27 );
	getvvstring( kmcpf.kvarsname,9, "macroname",10, &nc, kmacroname
	 ,MCPFN+1, nerr );
	apcmsg( kmacroname,MCPFN+1 );
	goto L_8888;


} /* end of function */

