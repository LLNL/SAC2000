#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/csf.h"
void /*FUNCTION*/ csinit(kmsg, ncmsg, nerr)
char *kmsg;
int ncmsg, *nerr;
{
	int j, j_, jcmsg, jmac, nleft, ntok2, ntokt;



	/*=====================================================================
	 * PURPOSE:  To initialize the "command stack" with a message.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kmsg:  Message to load into command stack. [c]
	 *   ncmsg:  Number of characters in kmsg. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:  1007
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/2
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
	 * ASSUMPTIONS:
	 *=====================================================================
	 * LIMITATIONS:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    810120:  Changed to output message retrieval from disk.
	 *    810429:  Changes needed for downloading commands from a program.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Initialize stack variables. */

	ntokt = 1;
	strcpy( kmcsf.kcs[ntokt - 1], kmcsf.kmac[0] );
	jmac = 0;

	/* - Break message into tokens. */

	tokens( kmsg, ncmsg, MTOK, (char*)kmcsf.ktok,9, &cmcsf.ntok, &jcmsg );
	cmcsf.ntok = cmcsf.ntok + 1;
	strcpy( kmcsf.ktok[cmcsf.ntok - 1], kmcsf.keoc[0] );

	/* - Check for adequate space in Command Stack (CS). */

	nleft = cmcsf.ncs - ntokt;
	ntok2 = cmcsf.ntok + 2;
	if( ntok2 > nleft ){
		*nerr = 1007;
		setmsg( "ERROR", *nerr );
		apcmsg( "Terminal input",15 );
		goto L_8888;
		}

	/* - Copy new tokens into CS. */

	for( j = 1; j <= cmcsf.ntok; j++ ){
		j_ = j - 1;
		strcpy( kmcsf.kcs[ntokt + j_], kmcsf.ktok[j_] );
		}
	ntokt = ntokt + cmcsf.ntok;

	/* - Add end-of-macro and end-of-stack tokens. */

	strcpy( kmcsf.kcs[ntokt - 1], kmcsf.kmac[1] );
	strcpy( kmcsf.kcs[ntokt], kmcsf.keocs );

	/* - Reinitilize indices and counters. */

	cmcsf.nlev = 0;

        fstrncpy( kmcsf.knmlev[0],MCPFN,"TERMINAL",8);

	Nlvusd[1] = 0;
	Nlvlen[1] = ntokt;
	cmcsf.ncur = 1;
	cmcsf.ntop = 0;
	cmcsf.nbot = ntokt + 1;
	cmcsf.lcsemp = FALSE;

L_8888:
	return;

} /* end of function */

