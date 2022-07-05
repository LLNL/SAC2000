#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ putvFILEptr(vars, vars_s, name, name_s, value, 
	 nerr)
char *vars;   int vars_s;
char *name;   int name_s;
FILE *value;
int *nerr;

{
	int i;
        char *tempptr;
        struct varsfile *flist;

	/*=====================================================================
	 * PURPOSE:  To store a vars FILE pointer.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:       Name of vars list. [c]
	 *    name:       Name of vars entry. [c]
	 *    value:      Value to store. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:       Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:   
	 *=====================================================================
	 * GLOBAL OUTPUT:
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

/* search for existing entry to replace */
        for (i=0; i<vfilelist.nentries; i++, flist++){
          if(strcmp(flist->varsname,vars) == 0) {
            if(strcmp(flist->variable,name) == 0) {
               /* replace existing entry */
              flist->value = value;
              goto L_8888;
            }
          }
        }

/* is there room for another entry? */
        if((vfilelist.nentries + 1) > vfilelist.nallocated) {
           tempptr = realloc(vfilelist.filelist,
                             (vfilelist.nentries+NVFILEINC)*sizeof(struct varsfile));
           if(tempptr==NULL){
             *nerr = 1;
             goto L_8888;
	   }
           else {
             vfilelist.filelist = (struct varsfile *)tempptr;
             vfilelist.nallocated += NVFILEINC;
           }           
        }

/* store the new entry */
        flist = vfilelist.filelist + vfilelist.nentries;
        flist->varsname = strdup(vars);
        flist->variable = strdup(name);
        flist->value = value;
        vfilelist.nentries += 1;

L_8888:
	return;

} /* end of function */

