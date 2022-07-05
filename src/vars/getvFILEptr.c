#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
void /*FUNCTION*/ getvFILEptr(vars, vars_s, name, name_s, value, 
	 nerr)
char *vars;   int vars_s;
char *name;   int name_s;
FILE **value;
int *nerr;
{
	int i;
        struct varsfile *flist;

	/*=====================================================================
	 * PURPOSE:  To get a vars FILE pointer.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:       Name of vars list. [c]
	 *    name:       Name of vars entry. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    value:      Value of vars FILE pointer. [i]
	 *    nerr:       Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 * 
	 * 
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 * 
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    121493:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
        flist = vfilelist.filelist;

/* search for entry */
        for (i=0; i<vfilelist.nentries; i++, flist++){
          if(strcmp(flist->varsname,vars) == 0) {
            if(strcmp(flist->variable,name) == 0) {
              *value = flist->value;
              goto L_8888;
            }
          }
        }

/* if we got here we didnt find it */
        *nerr = 1;
        
L_8888:
	return;

} /* end of function */

