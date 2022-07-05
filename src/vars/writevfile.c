#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "vars.h"
#include "nvars.h"
#include "mem.h"

void zputc(char* str,int strlen,int* array,int pnumc);

void /*FUNCTION*/ writevfile(vars, vars_s, file, nerr)
char *vars;   int vars_s;
char *file;
int *nerr;
{
	char filename[MAXCVNAME+1];
	int lopen;
	int index, itemp[2], jvars, ncerr, nlocdisk, 
	 numwords, nun;
	void /*zputc(),*/ zwabs();

	int *const Itemp = &itemp[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To write a vars list in memory to disk.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:    Name of vars list. [c]
	 *    file:    Name of disk file. [c]
	 *             Set to blanks if file name is to be the same as vars name.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    MAXCVNAME, VARSIDCODE, VARSLISTNOTFOUND, varslength, varsindex
	 *    mem:     sacmem
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  relamb
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    lopen:      Flag to know whether file has been opened or not. [l]
	 *    nun:        File unit number file is open on. [i]
	 *    jvars:      Index to requested vars list. [i]
	 *    numwords:   Number of words to copy from memory to disk file. [i]
	 *    nlocdisk:   Location (zero based) in disk file to start write. [i]   
	 *    index:      Index in sacmem array to start write. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890227:  Changed logic regarding file name and vars name.
	 *    870916:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870916
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	lopen = FALSE;

	/* - Make sure vars list exists. */

	if( !existsvlist( vars,vars_s, "MEMORY", &jvars ) ){
		*nerr = VARSLISTNOTFOUND;
		setmsg( "ERROR", *nerr );
		apcmsg( vars,vars_s );
		goto L_8888;
	}

	/* - Define name of file to write. */

	if( memcmp(file," ",1) == 0 ){
		strcpy( filename, kmvars.varsname[jvars - 1] );
	}
	else{
		fstrncpy( filename,MAXCVNAME, file, strlen(file));
	}

	/* - Open/create disk file. */

	znfile( &nun, filename,MAXCVNAME+1, "DATA",5, nerr );
	if( *nerr != 0 )
		goto L_8888;
	lopen = TRUE;

	/* - Write vars id code and version number to disk file to 
	 *   identify it as being in vars format. */

	nlocdisk = 0;
	numwords = 2;
	zputc( kmvars.varsidcode,5, &Itemp[1], 4 );
	Itemp[2] = VARSVERSION;
	zwabs( &nun, itemp, numwords, &nlocdisk, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Write vars list from memory to disk. */

	nlocdisk = nlocdisk + numwords;
	numwords = Varslength[jvars];
	index = Varsindex[jvars];
	zwabs( &nun, cmmem.sacmem[index], numwords, &nlocdisk, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Close disk file and return. */

L_8888:
	if( lopen )
		zclose( &nun, &ncerr );
	return;

} /* end of function */

