#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/cpf.h"
void /*FUNCTION*/ macrokeyword(kmacroargs, kmacroargs_s, nun, keys, 
	 keys_s, nerr)
char *kmacroargs;   int kmacroargs_s;
FILE *nun;
char *keys;   int keys_s;
int *nerr;
{
	char _c0[2], kdef[MCMSG+1], key[MCMSG+1], kline[MCMSG+1], kmacroname[MCPFN+1], 
	 kname[9], ktemp[MCMSG+1], ktoken[9], kval[MCMSG+1];
	int lfirst;
	int iatype, ic1, ic2, ica1, ica2, ick1, ick2, icv1, icv2, 
	 iktype, itype, jargs, jkeys, jline, nargs, nc, ncargs, ndef, 
	 nkey, nkeys, nline, nval, numsave;
	void *_p0;
        char *s1;


	/*=====================================================================
	 * PURPOSE: To process a "keyworded"  SAC macro file preamble.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kmacroargs:   Arguments from macro execute line. [c]
	 *    nun:          Fortran file unit that macro file is open on. [i]
	 *    keys:         List of keywords from macro preamble. [c]
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
	 *    880404:  Now lowercasing each token into a temporary string
	 *             before comparing to keywords.
	 *    870915:  Added option to skip over blank and comment lines.
	 *    870724:  Fixed bug that occurred  when there were no arguments.
	 *    870402:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870402
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Store the keyword list in the vars section. */

	nkeys = indexb( keys,keys_s );
	putvvstring( kmcpf.kvarsname,9, "keys",5, nkeys, keys,keys_s, 
	 nerr );

	/* - For each keyword in list, store it's name and give it
	 *   a "pseudo value" indicating that is does not have a "real value". */

	jkeys = 0;
L_1000:
	poptok( keys, nkeys, &jkeys, &ic1, &ic2, &itype );
	if( itype > 0 ){
		nkey = ic2 - ic1 + 1;
		fstrncpy( key, MCMSG, keys+ic1 - 1,ic2 - ic1 + 1);
                strncpy((s1=malloc(nkey+1)),key,nkey);
                s1[nkey] = '\0';

		putvvstring( kmcpf.kvarsname,9, s1, nkey + 1, MCPW, kmcpf.knoval,9, nerr );

		free(s1);
		if( *nerr != 0 )
			goto L_8888;
		goto L_1000;
		}

	/* - For each default card in macro preamble:
	 *   (Format of a default card is: $DEFAULT keyword value)
	 *   If it is a legal keyword, store the default value in vars section.
	 *   Otherwise raise an error. */

L_2000:

        if(fgets( kline,MCMSG+1,nun)==NULL){
          if(feof(nun))goto L_8888;
          goto L_9000;
	}
        if(kline[(numsave=strlen(kline)-1)] == '\n') kline[numsave] = ' ';

	nline = indexb( kline,MCMSG+1 );
	if( nline <= 0 || kline[0] == '*' )
		goto L_2000;
	jline = 0;
	poptok( kline, nline, &jline, &ic1, &ic2, &itype );

        strncpy((s1=malloc(ic2-ic1+2)),kline+ic1-1, ic2-ic1+1);
        s1[ic2-ic1+1] = '\0';
	upcase( s1, ic2-ic1 + 1, ktoken,9 );
        free(s1);

	if( strcmp(ktoken,"$DEFAULT") == 0 ){
		poptok( kline, nline, &jline, &ic1, &ic2, &itype );
		ndef = ic2 - ic1 + 1;
		fstrncpy( kdef, MCMSG, kline+ic1 - 1,min(ic2,MCMSG)-ic1+1);
		nval = nline - jline + 1;
		fstrncpy( kval, MCMSG, kline+jline - 1,min(nline,MCMSG)-jline+1);
		jkeys = 0;
L_3000:
		poptok( keys, nkeys, &jkeys, &ic1, &ic2, &itype );
		if( itype > 0 ){
			if( memcmp(kdef,keys+ic1 - 
			 1,ic2 - ic1 + 1) == 0 ){
                                strncpy((s1=malloc(ic2-ic1+2)),keys+ic1 - 
				 1,ic2 - ic1 + 1);
                                s1[ic2-ic1+1] = '\0';

				putvvstring( kmcpf.kvarsname,9, s1,ic2 - ic1 + 2, nval, kval,MCMSG+1, 
				 nerr );

				free(s1);
				if( *nerr != 0 )
					goto L_8888;
				}
			else{
				goto L_3000;
				}
			}
		goto L_2000;
		}

	/* - Process argument line if any.  Each token here is either a keyword 
	 *   or part of the "value" of another keyword. */

	nargs = indexb( kmacroargs,kmacroargs_s );
	if( nargs > 0 ){
		lfirst = TRUE;
		jargs = 0;
L_4000:
		poptok( kmacroargs, nargs, &jargs, &ica1, &ica2, &iatype );
		if( iatype > 0 ){
			jkeys = 0;
L_5000:
			poptok( keys, nkeys, &jkeys, &ick1, &ick2, &iktype );
			if( iktype > 0 ){
				ncargs = ica2 - ica1 + 1;
                                strncpy((s1=malloc(ncargs+1)),kmacroargs+ica1 - 1,ncargs);
                                s1[ncargs] = '\0';
				modcase( FALSE, s1, ncargs, ktemp );
                                free(s1);
				if( memcmp(ktemp+0,keys+ick1 - 
				 1,ick2 - ick1 + 1) == 0 ){
					if( !lfirst ){
						icv2 = ica1 - 2;
						nval = icv2 - icv1 + 1;
						if( iatype == 3 ){
                                                        fstrncpy( kval, MCMSG,kmacroargs+icv1 - 
							 1,icv2 - icv1 + 1);
                                                        *(kval+icv2-icv1+1) = kmacroargs[icv1 - 1];  
							}
						else{
							fstrncpy( kval, MCMSG, kmacroargs+icv1 - 
							 1,icv2 - icv1 + 1);
							}
						putvvstring( kmcpf.kvarsname,9, key,MCMSG+1, nval, 
						 kval,MCMSG+1, nerr );
						if( *nerr != 0 )
							goto L_8888;
						}
					lfirst = FALSE;
					fstrncpy( key, MCMSG, keys+ick1 - 1,ick2 - ick1 + 1);
					icv1 = jargs;
					goto L_4000;
					}
				else{
					goto L_5000;
					}
				}
			else{
				goto L_4000;
				}
			}
		else{
			icv2 = nargs;
			nval = icv2 - icv1 + 1;
			if( iatype == 3 ){
                                fstrncpy( kval, MCMSG,kmacroargs+icv1 - 
				 1,icv2 - icv1 + 1);
                                *(kval+icv2 - icv1 + 1) = kmacroargs[icv1 - 1]; 
				}
			else{
				fstrncpy( kval, MCMSG, kmacroargs+icv1 - 1,icv2 - 
				 icv1 + 1);
				}
			putvvstring( kmcpf.kvarsname,9, key,MCMSG+1, nval, kval,MCMSG+1, 
			 nerr );
			if( *nerr != 0 )
				goto L_8888;
			}
		}

L_8888:

	return;

L_9000:
	*nerr = 114;
	setmsg( "ERROR", *nerr );
	apcmsg( "macro keyword preamble for",27 );
	getvvstring( kmcpf.kvarsname,9, "macroname",10, &nc, kmacroname
	 ,MCPFN+1, nerr );
	apcmsg( kmacroname,MCPFN+1 );
	goto L_8888;


} /* end of function */

