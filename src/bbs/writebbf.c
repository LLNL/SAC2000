#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/bbs.h"
void /*FUNCTION*/ writebbf(kname, nerr, kname_s)
char *kname;    int kname_s ;
int *nerr;
{
	int nc ;

	/*=====================================================================
	 * PURPOSE:  To write a blackboard variable file from a program.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kname:   Name of blackboard variable file. [c]
	 *             Set to blanks to use previous blackboard file name.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  bbs/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    bbs:     knmbbs
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  writevfile
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871012:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  871012
	 *===================================================================== */
	/* - Write file to disk using the blackboard reserved name. */
#ifdef _LINUX
	nc = indexb (kname, kname_s) ;
	kname[ nc ] = '\0' ;
#endif
	writevfile( kmbbs.knmbbs,MCPFN+1, kname, nerr );

L_8888:
	return;

} /* end of function */





/* Added for FORTRAN friendliness */
void writebbf_ (kname, nerr, kname_s)
char *kname;    int kname_s ;
int *nerr;
{
	writebbf(kname, nerr, kname_s) ;
}
