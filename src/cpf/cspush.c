#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/csf.h"
void /*FUNCTION*/ cspush(ninvok, nerr)
int ninvok, *nerr;
{
	char kmsg[MCMSG+1];
	int larg, lnumbr, ltext;
	int j, j2, j_, jarg1, jcmsg, jcs, jtok, jtok_, junk, 
	 ncmsg, newcur, nlcbot, ntoksave, ntokt, toklen;
        int numchar;
	float fnumb;
        FILE *nun;


	/*=====================================================================
	 * PURPOSE:  To load a "old-style" command file onto command stack.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  COM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCMSG
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * ASSUMPTIONS:
	 *=====================================================================
	 * LIMITATIONS:
	 *=====================================================================
	 * KNOWN ERRORS:
	 *=====================================================================
	 * EXTERNAL DEPENDENCIES:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* DETERMINE NAME OF MACRO TO BE LOADED */

	sctok( ninvok + 1 );
	if( !lcchar( MCPFN, kmcsf.knmmac,MCPFN+1, &junk ) ){
	    *nerr = 1008;
	    setmsg( "ERROR", *nerr );
	    goto L_8888;
	}

	/* STORE THE CALLING ARGUMENTS FOR THIS MACRO */

	jarg1 = 1;
	while ( lcmore( nerr ) ){
	    if( lctok( (char*)kmcsf.karg1[jarg1 - 1],9, &lnumbr, &fnumb ) ){
		ictok( 1 );
		jarg1 = jarg1 + 1;
	    }
	}
	cmcsf.narg1 = jarg1 - 1;
	if( cmcsf.narg1 > cmcsf.narg ){
	    *nerr = 1009;
	    setmsg( "ERROR", *nerr );
	    apcmsg( "invocation:",12 );
	    apimsg( cmcsf.narg );
	    goto L_8888;
	}
	else if( cmcsf.narg1 < cmcsf.narg ){
	    for( jarg1 = cmcsf.narg1 ; jarg1 < cmcsf.narg; jarg1++ ){
		strcpy( kmcsf.karg1[jarg1], kmcsf.kquery );
	    }
	}


	/* OPEN DISK FILE */

	zopens( &nun, kmcsf.knmmac,MCPFN+1, "TEXT",5, nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* SAVE CERTAIN PORTIONS OF CURRENT CS BEFORE LOADING NEW MACRO. */

	/*    MOVE USEFUL PORTION OF CS ABOVE NEW MACRO TO TOP OF CS. */

	if( cmcsf.nmvtop > 0 ){
	    for( j = 0; j <= (cmcsf.nmvtop - 1); j++ ){
		strcpy( kmcsf.kcs[j], kmcsf.kcs[cmcsf.ntop + j - 1] );
	    }
	    cmcsf.ntop = 1;
	}
	newcur = cmcsf.nmvtop + 1;

	/*    MOVE PORTION OF CS BELOW NEW MACRO TO BOTTOM OF CS. */

	if( cmcsf.nmvbot > 0 ){
	    j2 = cmcsf.ncur + cmcsf.nmvbot - 1;
	    for( j = 0; j <= (cmcsf.nmvbot - 1); j++ ){
		strcpy( kmcsf.kcs[cmcsf.ncs - j - 1], kmcsf.kcs[j2 - j - 1]);
	    }
	}
	nlcbot = cmcsf.ncs - cmcsf.nmvbot;

	/* GET NEXT LINE FROM DISK FILE */

	jcs = newcur;
	larg = TRUE;
	ltext = FALSE;
	cmcsf.narg2 = 0;

        while ( (fgets(kmsg,MCMSG,nun) != NULL) || !(feof(nun)) ) {
            if(kmsg[(numchar=strlen(kmsg)-1)] == '\n')kmsg[numchar] = '\0';

	    ncmsg = indexb( kmsg,MCMSG+1 );

	    /* BREAK MESSAGE INTO TOKENS */

	    tokens( kmsg, ncmsg, MTOK, (char*)kmcsf.ktok,9, &cmcsf.ntok, &jcmsg );

	    /* CHECK FOR COMMENT TOKEN. NO TOKENS AFTER IT ARE PROCESSED. */
	    ntoksave = cmcsf.ntok;

	    for( jtok = 1; jtok <= cmcsf.ntok; jtok++ ){
		jtok_ = jtok - 1;
		if( memcmp(kmcsf.ktok[jtok_],kmcsf.kcmt,min(strlen(kmcsf.ktok[jtok_]),
		  strlen(kmcsf.kcmt))) == 0 ){
		    ntoksave = jtok - 1;
		    break ;
		}
	    }

	    cmcsf.ntok = ntoksave;

	    /* LOOK AT EACH TOKEN, CHECKING SYNTAX AND STORING THEM IN THE CS. */

	    for( jtok = 1; jtok <= cmcsf.ntok; jtok++ ){
		jtok_ = jtok - 1;
		if( larg ){
		    toklen = strlen(kmcsf.ktok[jtok_]);
		    if( memcmp(kmcsf.ktok[jtok_],kmcsf.kmac[0],min(toklen,
		      strlen(kmcsf.kmac[0]))) == 0 ){
			larg = FALSE;
			ltext = TRUE;
			if( cmcsf.narg1 > cmcsf.narg2 ){
			    *nerr = 1010;
			    setmsg( "ERROR", *nerr );
			    apimsg( cmcsf.narg1 );
			    apimsg( cmcsf.narg2 );
			    goto L_8888;
			}
			strcpy( kmcsf.kcs[jcs - 1], kmcsf.ktok[jtok_] );
			jcs = jcs + 1;
			if( jcs > nlcbot ){
			    *nerr = 1007;
			    setmsg( "ERROR", *nerr );
			    apcmsg( kmcsf.knmmac,MCPFN+1 );
			    goto L_8888;
			}
		    }
		    else if( memcmp(kmcsf.ktok[jtok_],kmcsf.kmac[1],min(toklen,
		      strlen(kmcsf.kmac[1]))) == 0 ){
			*nerr = 1011;
			setmsg( "ERROR", *nerr );
			goto L_8888;
		    }
		    else{
			cmcsf.narg2 = cmcsf.narg2 + 1;
			if( cmcsf.narg2 > cmcsf.narg ){
			    *nerr = 1009;
			    setmsg( "ERROR", *nerr );
			    apcmsg( "definition:",12 );
			    apimsg( cmcsf.narg );
			    goto L_8888;
			}
			strcpy( kmcsf.karg2[cmcsf.narg2 - 1], kmcsf.ktok[jtok_]);
		    }
		} /* end if( larg ) */
		else if( ltext ){
		    if( memcmp(kmcsf.ktok[jtok_],kmcsf.kmac[1],min(toklen,
                      strlen(kmcsf.kmac[1]))) == 0 ){
			ltext = FALSE;
		    }
		    else if( memcmp(kmcsf.ktok[jtok_],kmcsf.kmac[0],min(toklen,
                      strlen(kmcsf.kmac[0]))) == 0 ){
			*nerr = 1011;
			setmsg( "ERROR", *nerr );
			goto L_8888;
		    }
		    strcpy( kmcsf.kcs[jcs - 1], kmcsf.ktok[jtok_] );
		    jcs = jcs + 1;
		    if( jcs > nlcbot ){
			*nerr = 1007;
			setmsg( "ERROR", *nerr );
			apcmsg( kmcsf.knmmac,MCPFN+1 );
			goto L_8888;
		    }
		} /* end end if ( ltext ) */
	    } /* end for ( jtok ) */

	    /* LOOP BACK FOR MORE INPUT IF NO END-OF-MACRO TOKEN HAS BEEN FOUND */

	    if( larg )
		continue ;
	    if( ltext ){
		strcpy( kmcsf.kcs[jcs - 1], kmcsf.keoc[0] );
		jcs = jcs + 1;
	    }
	    else
		break ;
	} /* end while */

	/* - Come to here when macro file has been read.  This can happen when
	 *   the end-of-macro symbol [KMAC(2)] has been encountered in the file
	 *   or when a physical end-of-file was found during the READ. */
	ntokt = jcs - newcur;

	/* MOVE BOTTOM PORTION OF CS BACK TO LOCATION RIGHT AFTER LAST TOKEN IN
	 * NEW MACRO.  WE THEN HAVE A CONTINUOUS CS. */

	if( cmcsf.nmvbot > 0 ){
	    j2 = cmcsf.ncs - cmcsf.nmvbot + 1;
	    for( j = 0; j < cmcsf.nmvbot ; j++ ){
		j_ = j - 1;
		strcpy( kmcsf.kcs[jcs + j_], kmcsf.kcs[j2 + j_] );
	    }
	}

	/* UPDATE INDICES AND COUNTERS; CLOSE DISK FILE */

	strcpy( kmcsf.knmlev[cmcsf.nlev], kmcsf.knmmac );
	Nlvusd[cmcsf.nlev + 1] = 0;
	Nlvlen[cmcsf.nlev + 1] = ntokt;
	cmcsf.ncur = newcur;
	cmcsf.ntop = 0;
	cmcsf.nbot = cmcsf.nmvtop + ntokt + cmcsf.nmvbot;
	zcloses( &nun, nerr );

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    830817:  Replaced ZMSDSK with a fortran READ statement.
	 *    820122:  Made length of input message a machine parameter.
	 *    810205:  Replaced call to ZFILNM with more general ZCHSTR.
	 *    810120:  Changed to output message retrieval from disk.
	 *===================================================================== */

} /* end of function */

