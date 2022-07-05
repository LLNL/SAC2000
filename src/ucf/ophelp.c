#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ ophelp(kitem, kitem_s, nun, khfile, nerr)
char *kitem;   int kitem_s;
FILE **nun;
char *khfile ;
int *nerr;
{
	char khitem[30], khname[100], klfile[MCPFN+1];
        char kiline[MCMSG+1];
        char *iblank;
	int ncerr, ilen, idx;
	void zbasename();

	/*=====================================================================
	 * PURPOSE: To open a help package file, given its name.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KITEM:   The name of the help package [k].
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NUN:     The fortran file unit upon which the help package
	 *             file was opened. [i]
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1104
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    KDIRDL, KTYPDL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    OFM:     MINTL, KOUTM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ZGTFUN, CRNAME, ZOPEN, ZCLOSE
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    KHITEM:  The name of an item from the help list.
	 *    KHFILE:  The name of a help package file for an item.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870923:  Deleted ".saf" from aux file names.
	 *             Changed location of "contents" file.
	 *             Changed error return logic.
	 *    831110:  Generalized generation of pathname for HELP list.
	 *    820310:  Fixed minor bug involving auxiliary help packages.
	 *    800915:  Original version.
	 *    810115:  Major revision for new help package format.
	 *    810306:  Modified call to ZOPEN to open files read only.
	 *    810316:  Added indexed search to find correct help file.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870923
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

        for( idx = 0 ; idx < MCPFN ; idx++ )
            klfile[ idx ] = ' ' ;
        klfile[ MCPFN ] = '\0' ;

	/* - Create pathname and open file that contains list of help packages. */

	zbasename( klfile,MCPFN+1 );
	crname( klfile,MCPFN+1, KSUBDL, "help",5, nerr );
	if( *nerr != 0 )
		goto L_8888;
	crname( klfile,MCPFN+1, KDIRDL, "contents",9, nerr );
	if( *nerr != 0 )
		goto L_8888;
	zopens( nun, klfile,MCPFN+1, "ROTEXT",7, nerr );
	if( *nerr != 0 ){
		goto L_8000;
		}

	/* - For each item in help list file: */

L_1000:
        if(fgets(kiline,MCMSG+1,*nun)==NULL) goto L_8000;
        if(sscanf(kiline," %s %s", khitem, khname) != 2){
          goto L_8000;
	}

	/* -- If it matches the requested help package name, close
	 *    the help list file, open the help package, and return. */

        
	if( strcmp(kitem,khitem) == 0 ){
		zcloses( nun, nerr );
		zbasename( khfile,MCPFN+1 );
		crname( khfile,MCPFN+1, KSUBDL, "help",5, nerr );
		if( *nerr != 0 )
			goto L_8888;
		crname( khfile,MCPFN+1, KDIRDL, khname,30, nerr );
		if( *nerr != 0 )
			goto L_8888;
		zopens( nun, khfile,MCPFN+1, "ROTEXT",7, nerr );
		goto L_8888;
		}

	/* -- If not loop back for next item in help list file. */

	goto L_1000;

	/* - Set error flag if no help file was found. */

L_8000:
	zcloses( nun, &ncerr );
	*nun = NULL ;

L_8888:
	return;

} /* end of function */

