#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/msg.h"
void /*FUNCTION*/ sacmsg(nerr)
int *nerr;
{
	char kfile[MCPFN+1], kiline[MCMSG+1];
	int  idx ;
	int ioerr, ntused, numsave;
        FILE *nun;
	void zbasename();

	/*=====================================================================
	 * PURPOSE:  To read the SAC message file from disk to common.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag.
	 *=====================================================================
	 * MODULE/LEVEL:  MSG/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    KDIRDL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    MSG:     NFMSG, IFMSG, KFMSG
	 *=====================================================================
	 * GLOBAL COUPLING:  See SETMSG
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870923:  Deleted ".saf" suffixes from aux files.
	 *    870527:  Reworked file reading logic to make it compatible
	 *             with the current version of the MASSCOMP f77 compiler.
	 *    860203:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860203
	 *===================================================================== */
	/* PROCEDURE: */

	/* - Build the pathname and open the file containing output messages. */

        /*memset(kfile,' ',MCPFN);*/
        for( idx = 0 ; idx < MCPFN ; idx++ )
	    kfile[ idx ] = ' ' ;
        kfile[MCPFN] = '\0';

	zbasename( kfile,MCPFN+1 );
	crname( kfile,MCPFN+1, KDIRDL, "messages",9, nerr );
	if( *nerr != 0 )
		goto L_4000;
	zopens( &nun, kfile,MCPFN+1, "ROTEXT",7, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Read each message from disk file into common. */

L_2000:
	if( cmmsg.nfmsg < MFMSG ){
		cmmsg.nfmsg = cmmsg.nfmsg + 1;

                if(fgets(kiline,MCMSG,nun)==NULL){
                  if(feof(nun)) goto L_2020;
                  goto L_2010;
		}
                if(kiline[(numsave=strlen(kiline)-1)] == '\n') kiline[numsave] = ' ';

                if(sscanf(kiline,"%4d",  &Ifmsg[cmmsg.nfmsg]) != 1){
                  printf("error reading SAC message file-sacmsg\n");
                  goto L_2010;
	        }
                strcpy( kmmsg.kfmsg[cmmsg.nfmsg - 1],kiline+5);

		goto L_2000;
L_2010:
		*nerr = 100;
		setmsg( "ERROR", *nerr );
		apimsg( ioerr );
		apcmsgnum( 114 );
		apcmsg( kfile,MCPFN+1 );
L_2020:
		cmmsg.nfmsg = cmmsg.nfmsg - 1;
		}
	else{
		*nerr = 919;
		setmsg( "ERROR", *nerr );
		}

L_4000:
	zcloses( &nun, &ntused );

L_8888:
	return;

} /* end of function */

