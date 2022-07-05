#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ zauxfile(ksub, ksub_s, kfile, kfile_s, nerr)
char *ksub;   int ksub_s;
char *kfile;   int kfile_s;
int *nerr;
{
	char kdirpart[MCPFN+1], kfilepart[MCPFN+1], kgfile[MCPFN+1], kmsg[MCMSG+1];
	int lexist;
	short int _i0;
	int nc, nc1, nc2, idx ;
	void zbasename(), zgpmsg(), zsysop();

	/*=====================================================================
	 * PURPOSE:  To install a file in the SAC auxiliary directory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *      ksub:  The name of the auxiliary subdirectory for file. [c]
	 *      kfile: The name of the file to copy. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:  CO/5
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCPFN, MCMSG
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871222:  File inquiry moved to zinquire.
	 *    870921:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870921
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	for( idx = 0 ; idx < MCPFN ; idx++ )
	    kgfile[ idx ] = ' ' ;
        kgfile[ MCPFN ] = '\0' ;

	/* - Make sure file exists. */

	zinquire( kfile, &lexist );

	if( !lexist ){
		*nerr = 108;
		setmsg( "ERROR", *nerr );
		apcmsg( kfile,kfile_s );
		goto L_8888;
		}

	/* - Build global file name. */

	getdir( kfile,kfile_s, kdirpart,MCPFN+1, kfilepart,MCPFN+1 );
	zbasename( kgfile,MCPFN+1 );
	crname( kgfile,MCPFN+1, KSUBDL, ksub,ksub_s, nerr );
	if( *nerr != 0 )
		goto L_8888;
	crname( kgfile,MCPFN+1, KDIRDL, kfilepart,MCPFN+1, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Check to see that a global macro by this name does not already exist. */

	zinquire( kgfile, &lexist );
	if( lexist ){
                fprintf(MUNOUT," Global file with this name already exists\n");
L_3000:
		zgpmsg( "Do you want to overwrite this file?  $",39, kmsg,MCMSG+1 );
		if( kmsg[0] == 'n' || kmsg[0] == 'N' ){
			*nerr = 109;
			setmsg( "ERROR", *nerr );
			apcmsg( kgfile,MCPFN+1 );
			goto L_8888;
			}
		else if( kmsg[0] != 'y' && kmsg[0] != 'Y' ){
                        fprintf(MUNOUT,"  Please answer with a \"YES\" or \"NO\".\n");
			goto L_3000;
			}
		}

	/* - Copy input file to global directory.
	 *   UNIX Implementation:  use zsysop to first run "cp" to copy file
	 *   and then run "chmod" to change protection modes so anyone can use it. */

	nc1 = indexb( kfile,kfile_s );
	nc2 = indexb( kgfile,MCPFN+1 );

        memcpy(kmsg,"cp ",3);
        memcpy(kmsg+3,kfile,nc1);
        kmsg[nc1+3] = ' ';
        memcpy(kmsg+nc1+4,kgfile,nc2);
        kmsg[nc1+nc2+4] = '\0';

	nc = indexb( kmsg,MCMSG+1 );
	zsysop( kmsg,MCMSG+1, &nc, nerr );

        memcpy(kmsg,"chmod a=rwx ",12);
        memcpy(kmsg+12,kgfile,nc2);
        kmsg[nc2+12] = '\0';

	nc = indexb( kmsg,MCMSG+1 );
	zsysop( kmsg,MCMSG+1, &nc, nerr );

L_8888:

	return;


} /* end of function */

