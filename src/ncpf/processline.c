#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#define	MFUNCTIONS	10

#include "../../inc/mach.h"
#include "../../inc/exm.h"
#include "../../inc/cpf.h"
void /*FUNCTION*/ processline(kname, kname_s, kiline, kiline_s, niline, 
	 koline, koline_s, noline, nerr)
char *kname;   int kname_s;
char *kiline;   int kiline_s;
int niline;
char *koline;   int koline_s;
int *noline, *nerr;
{
	char _c0[2], k[9], kpromt[MCMSG+1], kvalue[1001];
	int larg, lbb, lhdr;
	int iarg1, iarg2, ibb1, ibb2, ibegin, ifunctionbegin[MFUNCTIONS], 
	 ihdr1, ihdr2, jiline, nc, nfunctions, notused, nvalue;
	void *_p0, zgpmsg();
        char *s1;


	int *const Ifunctionbegin = &ifunctionbegin[0] - 1;



	/*=====================================================================
	 * PURPOSE:  To "process" a line from a SAC macro file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kname:   Name of macro file. [c]
	 *    kname_s: Length of kname. [i]
	 *    kiline:  Input (raw) line. [c]
	 *    noline:  Length of kiline. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    koline:  Output (processed) line. [c]
	 *		ie: the input line is stripped of the information 
	 *		referring to blackboard variables, header variables
	 *		arguments, escapes, and inline functions; the result
	 *		is the output line.
	 *    noline:  Length of koline. [i]
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    cpf:     kbb, khdr, karg, kfunctionbegin, kfunctionend
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    k:       Character currently being processed. [c1]
	 *    jiline:  Index of character currently being processed. [i]
	 *    lbb:     .TRUE. if currently evaluating a blackboard token. [l]
	 *    lhdr:    .TRUE. if currently evaluating a header token. [l]
	 *    larg:    .TRUE. if currently evaluating an argument token. [l]
	 *    ibb1,2:  Pointers to beginning and end of blackboard token. [i]
	 *    ihdr1,2: Pointers to beginning and end of header token. [i]
	 *    iarg1,2: Pointers to beginning and end of argument token. [i]
	 *    kvalue:  Result of the evaluation of a blackboard, header
	 *             or argument token. [c]
	 *    nvalue:  Length of kvalue without trailing blanks.
	 *    MFUNCTIONS:     Maximum number of nested inline functions. [ip]
	 *    nfunctions:     Current number of nested inline functions. [i]
	 *    ifunctionbegin: Pointer in koline to beginning of inline function. [ia]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920504:  Added first word test to ignore SAC's prompt.
	 *             Added include file exm, changed local variable kprmt
	 *             to kpromp.
	 *    890127:  Added escape character option.
	 *    881228:  Added inline function expansion capability.
	 *             Added error return flag to input line.
	 *    871109:  Increased size of "kvalue".
	 *    871103:  Not doing an immediate return on error from gethv.
	 *             Now it encodes ERROR of UNDEFINED in command string.
	 *    870730:  Fixed bug when special character was last one in line.
	 *    870402:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881228
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Initialize counters and flags: */

	jiline = 1;
	lbb = FALSE;
	lhdr = FALSE;
	larg = FALSE;
	*noline = 0;

	fstrncpy( koline, koline_s-1, " ", 1 );  /* fill koline with spaces */
        koline[koline_s-1] = '\0';		 /* and terminate with '\0'. */

	nfunctions = 0;

	/* - Test first token to see if we need to ignore SAC's prompt,
	 *   convenient when using the mouse to repeat commands previously entered. */

	nc = indexb( kmexm.kprmt,13 );
	nc = nc - 1;			/* nc gets the number of chars in the prompt. */
	if( memcmp(kiline,kmexm.kprmt,min(nc,12)) == 0 )  /* if the prompt is in the msg */
		jiline = nc + 1;			  /* skip over it. */

	/* - Loop on each character in input line. */
	/* look for keys that indicate a blackboard, header, or argument variable, or an
	 * escape key, or an inline function.  If it's one of these, process it, 
	 * else just write it to the output string and go on to the next one. */

	/* the keys are in kmcpf, and they are initialized in ncpf/inicpf.c */

	while ( jiline <= niline ){
                fstrncpy(k, 8, " ", 1);		/* fill k with spaces */
                k[0] = kiline[jiline - 1];

		/* -- If escape character, then next character is copied without interpretation. */
		if( memcmp(&kmcpf.kescape,k,1) == 0 ){
			jiline = jiline + 1;
                        k[0] = kiline[jiline - 1];
			*noline = *noline + 1;
			koline[*noline - 1] = ichar(k);
		}

		/* -- If blackboard variable key. */
		else if( memcmp(&kmcpf.kbb,k,1) == 0 || (lbb && (strcmp(k,
		 "        ") == 0 || jiline == niline)) ){
			lbb = !lbb;
			if( lbb ){
				ibb1 = jiline + 1;
			}
			else{
				if( (memcmp(&kmcpf.kbb,k,1) == 0 || strcmp(k,"        ") == 
				 0) || jiline != niline ){
					ibb2 = jiline - 1;
				}
				else{
					ibb2 = niline;
				}

                                strncpy((s1=malloc(ibb2 - ibb1 + 2)),kiline+ibb1 - 1,ibb2 - ibb1 + 1);
                                s1[ibb2 - ibb1 + 1] = '\0';

				getbbv( s1, kvalue, nerr, ibb2-ibb1 + 1, 1000 );

				free(s1);

				if( *nerr != 0 )
					goto L_8888;
				nvalue = indexb( kvalue,1001 );
				*noline = *noline + 1;
				subscpy( koline, *noline - 1, *noline + nvalue - 
				 2, koline_s - 1, kvalue );
				*noline = *noline + nvalue - 1;
				if( strcmp(k,"        ") == 0 ){
					*noline = *noline + 1;
					koline[*noline - 1] = ' ';
				}
			} /* end else associated with if( lbb ) */
		} /* end blackboard variable key */

		/* -- If header variable key. */
		else if( memcmp(&kmcpf.khdr,k,1) == 0 || (lhdr && (strcmp(k
		 ,"        ") == 0 || jiline == niline)) ){
			lhdr = !lhdr;
			if( lhdr ){
				ihdr1 = jiline + 1;
			}
			else{
				if( (memcmp(&kmcpf.khdr,k,1) == 0 || strcmp(k,"        ") == 
				 0) || jiline != niline ){
					ihdr2 = jiline - 1;
				}
				else{
					ihdr2 = niline;
				}

                                strncpy((s1=malloc(ihdr2-ihdr1+2)),kiline+ihdr1-1,ihdr2-ihdr1+1); 
				s1[ihdr2-ihdr1+1] = '\0'; 
				gethv( s1, ihdr2 - ihdr1 + 2, kvalue,1001, &notused );
				free(s1);

				nvalue = indexb( kvalue,1001 );
				*noline = *noline + 1;
				subscpy( koline, *noline - 1, *noline + nvalue - 
				 2, koline_s - 1, kvalue );
				*noline = *noline + nvalue - 1;
				if( strcmp(k,"        ") == 0 ){
					*noline = *noline + 1;
					koline[*noline - 1] = ' ';
				}
			}
		} /* end header variable key. */

		/* -- If argument key. */
		else if( memcmp(&kmcpf.karg,k,1) == 0 || (larg && (strcmp(k
		 ,"        ") == 0 || jiline == niline)) ){
			larg = !larg;
			if( larg ){
				iarg1 = jiline + 1;
			}
			else{
				if( (memcmp(&kmcpf.karg,k,1) == 0 || strcmp(k,"        ") == 
				 0) || jiline != niline ){
					iarg2 = jiline - 1;
				}
				else{
					iarg2 = niline;
				}

                                strncpy((s1=malloc(iarg2-iarg1+2)),kiline+iarg1 - 
				                              1,iarg2 - iarg1 + 1);
                                s1[iarg2 - iarg1 + 1] = '\0';

				getvvstring( kname,kname_s, s1, iarg2 - iarg1 + 2, &nvalue, 
				 kvalue,1001, nerr );

				free(s1);

				if( *nerr != 0 || memcmp(kvalue+0,kmcpf.knoval,MIN(MCPW,1000)) == 0 ){
                                        fstrncpy( kpromt, MCMSG, kiline+iarg1 - 
					 1,iarg2 - iarg1 + 1);
                                        memcpy(kpromt+(iarg2 - iarg1 + 1),"?  $",4); 
					zgpmsg( kpromt,MCMSG+1, kvalue,1001 );
					nvalue = indexb( kvalue,1001 );

                                        strncpy((s1=malloc(iarg2-iarg1+2)),kiline+iarg1 - 
					 1,iarg2 - iarg1 + 1);
                                        s1[iarg2 - iarg1 + 1] = '\0';

					putvvstring( kname,kname_s, s1, iarg2 - iarg1 + 2, nvalue, 
					 kvalue,1001, nerr );
					
					free(s1);

					if( *nerr != 0 )
						goto L_8888;
				}
				else{
					nvalue = indexb( kvalue,1001 );
				}
				*noline = *noline + 1;

                                strncpy((s1=malloc(nvalue+1)),kvalue,nvalue);
                                s1[nvalue] = '\0';
				subscpy( koline, *noline - 1, *noline + nvalue - 
				 2, koline_s - 1, s1 );
                                free(s1);
				*noline = *noline + nvalue - 1;
				if( strcmp(k,"        ") == 0 ){
					*noline = *noline + 1;
					koline[*noline - 1] = ' ';
				}
			}
		} /* end argument key */

		/* -- If beginning of inline function, save pointer to output line. */
		else if( memcmp(&kmcpf.kfunctionbegin,k,1) == 0 ){
			if( nfunctions < MFUNCTIONS ){
				nfunctions = nfunctions + 1;
				Ifunctionbegin[nfunctions] = *noline + 1;
			}
			else{
				*nerr = 1018;
				setmsg( "ERROR", *nerr );
				apimsg( MFUNCTIONS );
				goto L_8888;
			}
		}

		/* -- If end of inline function, process function to determine its value. */
		else if( memcmp(&kmcpf.kfunctionend,k,1) == 0 ){
			if( nfunctions > 0 ){
				ibegin = Ifunctionbegin[nfunctions];

                                strncpy((s1=malloc(*noline-ibegin + 2)),koline+ibegin - 1,*noline - 
				 ibegin + 1);
                                s1[*noline-ibegin+1] = '\0';

				processfunction( s1, *noline - ibegin + 2, kvalue,1001, nerr );
				
				free(s1);
				if( *nerr != 0 )
					goto L_8888;
				nvalue = indexb( kvalue,1001 );

                                strncpy((s1=malloc(nvalue+1)),kvalue,nvalue);
                                s1[nvalue] = '\0';

				subscpy( koline, ibegin - 1, -1, koline_s - 1, s1 );

                                free(s1);
				*noline = ibegin + nvalue - 1;
				nfunctions = nfunctions - 1;
			}
			else{
				*nerr = 1019;
				setmsg( "ERROR", *nerr );
				apcmsg( "too many RIGHT parentheses.",28 );
				goto L_8888;
			}
		}

		/* -- If building up a blackboard, header, or argument token, do nothing. */
		else if( (lbb || lhdr) || larg )
		{ /* do nothing */ }

		/* -- Otherwise, simply copy character to output string. */
		else{
			*noline = *noline + 1;
		/* --- Removed conversion from single to double quotes 
		       so that users could use unix system command like 
                       awk that need single quotes' */
		/*   	if ( ichar(k) == '\'' )
				koline[*noline - 1] = '\"' ;
			else */
			koline[*noline - 1] = ichar(k);
		}

		/* -- Increment character pointer and loop until input line is processed. */
		jiline = jiline + 1;

	} /* end while */

	if( nfunctions > 0 ){
		*nerr = 1019;
		setmsg( "ERROR", *nerr );
		apcmsg( "too many LEFT parentheses.",27 );
		goto L_8888;
	}

        strcpy(kmcpf.kinputline,koline);  /* save the processed input line. */

L_8888:

	return;

} /* end of function */

