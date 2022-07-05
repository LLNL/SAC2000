#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#define	MCPRMT	(MCPW + 3)

#include "../../inc/mach.h"
#include "../../inc/csf.h"
#include "../../inc/exm.h"
void /*FUNCTION*/ cspop(kcom, kcom_s, mcom, ninvok, ncom, nerr)
char *kcom;   int kcom_s;
int mcom, *ninvok, *ncom, *nerr;
{
#define KCOM(I_,J_)	(kcom+(I_)*(kcom_s)+(J_))
	char kmsg[MCMSG+1], kname[MCMSG+1], kqprmt[12], kvalue[MCMSG+1];
	char kcscur[9] = "        " ;
	int jarg, jcom, kcscur_len, ncomx, nqprmt;
	void zgpmsg();
	static int natype = 0;
	static int nansw = 0;
	static int nchkpr = 0;

	/*=====================================================================
	 * PURPOSE:  To "pop" the next command off the command stack.
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
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820122:  Made length of input message a machine parameter.
	 *    810120:  Changed to output message retrieval from disk.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	jcom = *ncom;
	ncomx = *ncom;
	*ninvok = 0;

L_1000:
	strcpy( kcscur, kmcsf.kcs[cmcsf.ncur - 1] );
        kcscur_len = indexb(kcscur,9);

	jarg = nequal( kcscur, (char*)kmcsf.karg2,9, cmcsf.narg2 );
	if( !strcmp(kcscur,kmcsf.kpass)){
		jcom = jcom + 1;
                fstrncpy(  KCOM(jcom - 1,0),kcom_s - 1, kmcsf.kcs[cmcsf.ncur],strlen(kmcsf.kcs[cmcsf.ncur]));
		cmcsf.ncur = cmcsf.ncur + 2;
		Nlvusd[cmcsf.nlev] = Nlvusd[cmcsf.nlev] + 2;
		}
	else if( jarg > 0 ){
		if( memcmp(kmcsf.karg1[jarg - 1],kmcsf.kquery,min(strlen(kmcsf.karg1[jarg - 1]),
                                                                  strlen(kmcsf.kquery))) == 0 ){
			nqprmt = indexb( kcscur,9 );
                        fstrncpy( kqprmt ,11,kcscur,min(nqprmt,8));
                        memcpy(kqprmt+min(nqprmt,8),"? $",3);
			zgpmsg( kqprmt,12, kmsg,MCMSG+1 );
			strscpy( kmcsf.karg1[jarg - 1], kmsg, 8 );
			jcom = jcom + 1;
                        fstrncpy(  KCOM(jcom - 1,0),kcom_s-1,kmcsf.karg1[jarg - 1],
                                                      strlen(kmcsf.karg1[jarg - 1]));
			}
		else if( memcmp(kmcsf.karg1[jarg - 1],kmcsf.kdel,min(strlen(kmcsf.karg1[jarg - 1]),
                                                                     strlen(kmcsf.kdel))) != 0 ){
			jcom = jcom + 1;
                        fstrncpy(  KCOM(jcom - 1,0),kcom_s-1,kmcsf.karg1[jarg - 1],
                                                      strlen(kmcsf.karg1[jarg - 1]));
			}
		cmcsf.ncur = cmcsf.ncur + 1;
		Nlvusd[cmcsf.nlev] = Nlvusd[cmcsf.nlev] + 1;
		}
	else if( !strcmp(kcscur,kmcsf.kinv[0])){
		if( jcom > 0 ){
			*ncom = jcom;
			if( *ninvok > 0 )
				cmcsf.nmvbot = cmcsf.nbot - cmcsf.ncur + 1;
			goto L_8888;
			}
		else{
			jcom = jcom + 1;
                        fstrncpy(  KCOM(jcom - 1,0),kcom_s-1,kcscur,strlen(kcscur));
			*ninvok = jcom;
			if( cmcsf.ntop > 0 )
				cmcsf.nmvtop = cmcsf.ncur - cmcsf.ntop;
			cmcsf.ncur = cmcsf.ncur + 1;
			Nlvusd[cmcsf.nlev] = Nlvusd[cmcsf.nlev] + 1;
			}
		}
	else if( !strcmp(kcscur,kmcsf.kinv[1])){
		if( *ninvok > 0 ){
			cmcsf.nmvbot = cmcsf.nbot - cmcsf.ncur + 1;
			*ncom = jcom;
			goto L_8888;
			}
		else{
			jcom = jcom + 1;
                        fstrncpy(  KCOM(jcom - 1,0),kcom_s-1,kcscur,strlen(kcscur));
			*ninvok = jcom;
			if( cmcsf.ntop > 0 )
				cmcsf.nmvtop = cmcsf.ncur - cmcsf.ntop;
			cmcsf.ncur = cmcsf.ncur + 1;
			Nlvusd[cmcsf.nlev] = Nlvusd[cmcsf.nlev] + 1;
			}
		}
	else if( !strcmp(kcscur,kmcsf.kappnd)){
		cmcsf.ncur = cmcsf.ncur + 1;
		Nlvusd[cmcsf.nlev] = Nlvusd[cmcsf.nlev] + 1;
		}
	else if( !strcmp(kcscur,kmcsf.kmac[0])){
		cmcsf.nlev = cmcsf.nlev + 1;
		cmcsf.ncur = cmcsf.ncur + 1;
		Nlvusd[cmcsf.nlev] = 1;
		}
	else if( !strcmp(kcscur,kmcsf.kmac[1])){
		cmcsf.ncur = cmcsf.ncur + 1;
		Nlvusd[cmcsf.nlev] = Nlvusd[cmcsf.nlev] + 1;
		if( Nlvusd[cmcsf.nlev] != Nlvlen[cmcsf.nlev] ){
			*nerr = 901;
			setmsg( "ERROR", *nerr );
			apcmsg( "CSPOP #1",9 );
			goto L_8888;
			}
		else{
			Nlvusd[cmcsf.nlev] = 0;
			cmcsf.nlev = cmcsf.nlev - 1;
			if( cmcsf.nlev < 0 ){
				*nerr = 901;
				setmsg( "ERROR", *nerr );
				apcmsg( "CSPOP #2",9 );
				goto L_8888;
				}
			}
		}
	else if( !strcmp(kcscur,kmcsf.keocs) ){
		if( cmcsf.nlev != 0 ){
			*nerr = 901;
			setmsg( "ERROR", *nerr );
			apcmsg( "CSPOP #3",9 );
			goto L_8888;
			}
		else if( *ninvok <= 0 ){
			cmcsf.lcsemp = TRUE;
			cmcsf.ncur = cmcsf.ncur + 1;
			}
		*ncom = jcom;
		cmcsf.nmvbot = cmcsf.nbot - cmcsf.ncur + 1;
		goto L_8888;
		}
	else if( nequal( kcscur, (char*)kmcsf.keoc,9, 2 ) > 0 ){
		*ncom = jcom;
		if( *ninvok > 0 ){
			cmcsf.nmvbot = cmcsf.nbot - cmcsf.ncur + 1;
			}
		else{
			cmcsf.ncur = cmcsf.ncur + 1;
			Nlvusd[cmcsf.nlev] = Nlvusd[cmcsf.nlev] + 1;
			}
		goto L_8888;
		}
	else{
		jcom = jcom + 1;
                fstrncpy(  KCOM(jcom - 1,0),kcom_s-1,kcscur,strlen(kcscur));
		cmcsf.ncur = cmcsf.ncur + 1;
		Nlvusd[cmcsf.nlev] = Nlvusd[cmcsf.nlev] + 1;
		}

	goto L_1000;

L_8888:

	return;

#undef	KCOM
} /* end of function */

