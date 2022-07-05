#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/tpf.h"
void /*FUNCTION*/ tokens(kstrg, ncstrg, mtok, ktok, ktok_s, jtok, jcstrg)
char *kstrg;
int ncstrg, mtok;
char *ktok;   int ktok_s;
int *jtok, *jcstrg;
{
#define KTOK(I_,J_)	(ktok+(I_)*(ktok_s)+(J_))
	char ctostr[] = " ";	/* char to string */
	char kcurdl[9], knwtok[9];
	int lmsg, lnwtok;
	byte kchar, klchar;
	int check, imsgcc, imsgtp, itoksv, jnwtok, mtokm1;



	/*=====================================================================
	 * PURPOSE:  To break a character string into tokens.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kstrg:   Character string. [c]
	 *    ncstng:  Length of KSTRG. [i]
	 *    mtok:    Maximum number of tokens to return. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ktok:    Array containing tokens, left justified. [k]
	 *    jtok:    Number of tokens returned. [i]
	 *    jcstng:  Set to last character processed.  If this is less than
	 *             NCSTRG, then KTOK has been filled up before the end of
	 *             the string has been reached.
	 *=====================================================================
	 * MODULE/LEVEL:  tpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    tpf:     ntokdl, ktokdl, nmsgdl, kmsgdl
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     cnvita, ljust
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900518:  Forced minimum length of a message to be 1 character.
	 *             This fixed a problem with the VAX VMS version.
	 *    870730:  Deleted conversion of tokens to upper case.
	 *    811230:  Modified message delimiter logic.
	 *    800514:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED: 900518
	 *===================================================================== */
	/* PROCEDURE: */
	*jtok = 0;
	*jcstrg = 0;
	klchar = ' ';
	lmsg = FALSE;
	lnwtok = FALSE;
	jnwtok = 0;
	mtokm1 = mtok - 1;
	
	/* These were set to zero because lint revealed they were being used before set. No idea if 0 is correct. */
	imsgtp = 0; 
        imsgcc = 0;
	itoksv = 0;

L_1000:
	*jcstrg = *jcstrg + 1;
	if( *jcstrg <= ncstrg ){
	    kchar = kstrg[*jcstrg - 1];
	    if( lmsg ){
		if( imsgtp == 1 && memcmp(&kchar,kcurdl,1) == 0 ){
		    lmsg = FALSE;
		    if( jnwtok >= 1 ){
			*jtok = *jtok + 1;
			fstrncpy ( KTOK ( *jtok - 1 , 0 ) , ktok_s - 1 ,
				   knwtok , min ( jnwtok , 8 ) ) ;
			jnwtok = 0;
		    }
		    cnvita( max( imsgcc, 1 ), KTOK(itoksv - 1,0),ktok_s );
		    ljust( KTOK(itoksv - 1,0),ktok_s );
		}
		else{
		    check = imsgtp == 2 && nequal ( &kchar, &kmtpf.ktokdl ,
						    1 , cmtpf.ntokdl ) > 0 ;
		    if( check ){
			lmsg = FALSE;
			if( jnwtok >= 1 ){
			    *jtok = *jtok + 1;
			    fstrncpy ( KTOK ( *jtok - 1 , 0 ) , ktok_s - 1 ,
					knwtok , min ( jnwtok , 8 ) ) ;
			    jnwtok = 0;
			}
			cnvita( max( imsgcc, 1 ), KTOK(itoksv - 1,0),ktok_s );
			ljust( KTOK(itoksv - 1,0),ktok_s );
			*jtok = *jtok + 1;
			ctostr[0]=kchar;
			fstrncpy( KTOK(*jtok - 1,0),ktok_s-1,ctostr,1);
		    }
		    else if( imsgtp == 2 && kchar == ' ' ){
			lmsg = FALSE;
			if( jnwtok >= 1 ){
			    *jtok = *jtok + 1;
			    fstrncpy ( KTOK ( *jtok - 1 , 0 ) , ktok_s - 1 ,
					knwtok , min ( jnwtok , 8 ) ) ;
			    jnwtok = 0;
			}
			cnvita( max( imsgcc, 1 ), KTOK(itoksv - 1,0),ktok_s );
			ljust( KTOK(itoksv - 1,0),ktok_s );
		    }
		    else{
			jnwtok = jnwtok + 1;
			knwtok[jnwtok - 1] = kchar;
			if( jnwtok == MCPS ){
			    *jtok = *jtok + 1;
			    fstrncpy ( KTOK ( *jtok - 1 , 0 ) , ktok_s - 1 ,
					knwtok , min ( jnwtok , 8 ) ) ;
			    jnwtok = 0;
			}
			imsgcc = imsgcc + 1;
		    }
		}
	    }
	    else if( kchar == ' ' ){
		if( (klchar != ' ' && jnwtok >= 1) || lnwtok ){
		    *jtok = *jtok + 1;
		    fstrncpy( KTOK(*jtok - 1,0),ktok_s-1,knwtok,min(jnwtok,8));
		    jnwtok = 0;
		    lnwtok = FALSE;
		}
	    }
	    else{
		check = nequal( &kchar, &kmtpf.ktokdl,1, cmtpf.ntokdl ) > 0;
		if( check ){
		    if( jnwtok >= 1 ){
			*jtok = *jtok + 1;
			fstrncpy ( KTOK ( *jtok - 1 , 0 ) , ktok_s - 1 ,
					knwtok , min ( jnwtok , 8 ) ) ;
			jnwtok = 0;
		    }
		    *jtok = *jtok + 1;
		    ctostr[0]=kchar;
		    fstrncpy( KTOK(*jtok - 1,0),ktok_s-1,ctostr,1);
		    lnwtok = FALSE ;
		}
		else{
		    check = nequal( &kchar, &kmtpf.kmsgdl,1, cmtpf.nmsgdl ) > 0;
		    if( check ){
			if( jnwtok >= 1 ){
			    *jtok = *jtok + 1;
			    fstrncpy ( KTOK ( *jtok - 1 , 0 ) , ktok_s - 1 ,
					knwtok , min ( jnwtok , 8 ) ) ;
			    jnwtok = 0;
			}
			if( (*jtok + 3) >= mtokm1 )
			    goto L_8888;
			*jtok = *jtok + 1;
			ctostr[0]=kchar;
			fstrncpy( KTOK(*jtok - 1,0),ktok_s-1,ctostr,1);
			lmsg = TRUE;
			imsgtp = 1;
			fstrncpy( kcurdl,8,ctostr,1);
			*jtok = *jtok + 1;
			itoksv = *jtok;
			imsgcc = 0;
		    }
		    else if( lnwtok ){
			if( (*jtok + 3) >= mtokm1 )
			    goto L_8888;
			*jtok = *jtok + 3;
			fstrncpy ( KTOK ( *jtok - 1 , 0 ) , ktok_s - 1 ,
					knwtok , min ( jnwtok , 8 ) ) ;
			fstrncpy( KTOK(*jtok - 3,0),ktok_s-1, "'", 1 );
			lmsg = TRUE;
			imsgtp = 2;
			itoksv = *jtok - 1;
			jnwtok = 1;
			knwtok[jnwtok - 1] = kchar;
			imsgcc = MCPS + 1;
			lnwtok = FALSE;
		    }
		    else{
			jnwtok = jnwtok + 1;
			knwtok[jnwtok - 1] = kchar;
			if( jnwtok == MCPS )
			    lnwtok = TRUE;
		    }
		}
	    }
	    klchar = kchar;
	    if( *jtok < mtokm1 )
		goto L_1000;
	}
	if( jnwtok >= 1 ){
	    *jtok = *jtok + 1;
	    fstrncpy( KTOK(*jtok - 1,0),ktok_s-1,knwtok,min(jnwtok,8));
	    jnwtok = 0;
	}
	if( lmsg ){
	    cnvita( max( imsgcc, 1 ), KTOK(itoksv - 1,0),ktok_s );
	    ljust( KTOK(itoksv - 1,0),ktok_s );
	}

L_8888:
	return;

#undef	KTOK
} /* end of function */

