#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "complex.h"
#include "proto.h"
#include "vars.h"
#include "nvars.h"
#include "mem.h"

void apcmsg2(char* kalpha, int kalpha_s);


void /*FUNCTION*/ readvfile(fullvars, fullvars_s, node, nerr)
char *fullvars;   int fullvars_s;
int *node, *nerr;
{
	char varsid[5], varstest[5];
	int lopen;
	int index, itemp[10], ncerr, nderr, nilindex, nlocdisk, 
	 numwords, nun;
	void decodevdesc(), zgetc(), zrabs();
        char *s1;


	int *const Itemp = &itemp[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To read a vars list on disk to memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    fullvars:  Full (absolute) name of the vars disk file. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    node:      The vars storage system node number for this list. [i]
	 *    nerr:      Error return flag. [i]
	 *=====================================================================
	 * INTERNAL SUBROUTINE:  Not normally called by user.
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    VARSIDCODE, MAXCVNAME
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    varsindex, varslength
	 *    mem:     sacmem
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  zgtfun, zopen, zrabs, zgetc, 
	 *             setmsg, apcmsg, aplmsg, decodevdesc, deletevlist, 
	 *             createvlist, findvnil, zclose
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890227:  Changed argument list.
	 *    881115:  Added output of node number.
	 *    880321:  Included logic to find and save nil index after read.
	 *    870916:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870916
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	*node = 0;
	lopen = FALSE;

	/* - Open disk file. */

        strncpy((s1=malloc(fullvars_s-1)),fullvars+1,fullvars_s - 2);
        s1[fullvars_s - 2] = '\0';

	zopen_sac( &nun, s1, fullvars_s- 1, "DATA",5, nerr );

	free(s1);
	if( *nerr != 0 )
		goto L_8888;
	lopen = TRUE;

	/* - Read first few words of disk file to make sure it is in vars format
	 *   and to determine the length of the vars list. */

	nlocdisk = 0;
	numwords = 10;
	zrabs( &nun, itemp, numwords, &nlocdisk, nerr );
	if( *nerr != 0 )
		goto L_8888;
	zgetc( kmvars.varsidcode, varsid, 4 );
	zgetc( &Itemp[1], varstest, 4 );
	if( memcmp(varstest,varsid,4) != 0 ){
		*nerr = NOTVARSFILE;
		setmsg( "ERROR", *nerr );
                apcmsg2(fullvars+1,fullvars_s - 2);
		aplmsg( "VARS id is incorrect.",22 );
		goto L_8888;
	}
	if( Itemp[2] != VARSVERSION ){
		*nerr = NOTVARSFILE;
		setmsg( "ERROR", *nerr );
                apcmsg2(fullvars+1,fullvars_s - 2);
		aplmsg( "VARS version number is incorrect.",34 );
		goto L_8888;
	}
	decodevdesc( &Itemp[3], &cmvars.deleteflag, &cmvars.readonlyflag, 
	 &cmvars.indirectflag, &cmvars.sharedflag, &cmvars.reservedflag, 
	 &cmvars.applflag1, &cmvars.applflag2, &cmvars.valuetype, &cmvars.desclength, 
	 &cmvars.namelength, &cmvars.valuelength );
	if( cmvars.valuetype != VALUELIST ){
		*nerr = NOTVARSFILE;
		setmsg( "ERROR", *nerr );
                apcmsg2(fullvars+1,fullvars_s - 2);
		aplmsg( "First VARS entry or data list is missing.",42 );
		goto L_8888;
	}
	numwords = cmvars.desclength + cmvars.namelength + cmvars.valuelength;

	/* - Delete vars list in memory if it already exists. */

	deletevlist( fullvars,fullvars_s, "MEMORY", &nderr );

	/* - Create a new vars list. */

	createvlist( fullvars,fullvars_s, numwords, node, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Read vars list from disk to memory. */

	nlocdisk = 2;
	index = Varsindex[*node];
        cmvars.varsnode1 = index;

	zrabs( &nun, cmmem.sacmem[index], numwords, &nlocdisk, nerr );
	if( *nerr != 0 ){
                apcmsg2(fullvars+1,fullvars_s - 2);
		goto L_8888;
	}

	/* - Find the nil terminator index and save index. Turn off modified flag. */

	decodevdesc( cmmem.sacmem[index], &cmvars.deleteflag, &cmvars.readonlyflag, 
	 &cmvars.indirectflag, &cmvars.sharedflag, &cmvars.reservedflag, 
	 &cmvars.applflag1, &cmvars.applflag2, &cmvars.valuetype, &cmvars.desclength, 
	 &cmvars.namelength, &cmvars.valuelength );
	index = index + cmvars.desclength + cmvars.namelength;
	findvnil( index, &nilindex );
	Varsnilindex[*node] = nilindex;
	Varsmodified[*node] = FALSE;

	/* - Close disk file and return. */

L_8888:
	if( lopen )
		zclose( &nun, &ncerr );
	return;

} /* end of function */

