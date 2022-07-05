#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"

void  znfiles(FILE** nfu, char* kname, int kname_s, char* ktype, int ktype_s, int* nerr);

void /*FUNCTION*/ zrunname(name, name_s, args, args_s, nfun, runfile, 
	 runfile_s, nerr)
char *name;   int name_s;
char *args;   int args_s;
FILE **nfun;
char *runfile;   int runfile_s;
int *nerr;
{
	int ncargs, ncname ;
        char *strtemp1, *strtemp2;

	/*=====================================================================
	 * PURPOSE:  To create a file to run an external program.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *     name:    Name of the program to be run. [c]
	 *     args:    Execute line arguments to program. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *     nfun:    Fortran file unit that runfile is open on. [i]
	 *     runfile: Name of run file. [c]
	 *     nerr:    Error return flag.  Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  co/5
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871014:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  871014
	 *===================================================================== */
	/* PROCEDURE: */

	/* - Create a new file to contain the shell script. */

	ncname = indexb( name,name_s );

        memset(runfile,(int)' ',runfile_s - 1);
        runfile[runfile_s - 1] = '\0';
        memcpy(runfile,"sacrunfile",10);

	znfiles( nfun, runfile,runfile_s, "TEXT",5, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Write the first line to the file. */

	ncargs = max( 1, indexb( args,args_s ) );

        strtemp1 = malloc(ncname+1);
        strtemp2 = malloc(ncargs+1);
        strncpy(strtemp1,name,ncname);
        strtemp1[ncname] = '\0';
        strncpy(strtemp2,args,ncargs);
        strtemp2[ncargs] = '\0';

        fprintf(*nfun,"%s%c%s%s\n", strtemp1, ' ', strtemp2, " << endrun" );

        free(strtemp1);
        free(strtemp2);
L_8888:
	return;

} /* end of function */

