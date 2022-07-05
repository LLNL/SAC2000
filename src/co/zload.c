#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <dlfcn.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dload.h"
void /*FUNCTION*/ zload(kfile, index, nerr)
char *kfile;
int *index, *nerr;
{
         char solist[300], *env_solist, *soname, *funcname;
         char kfiletemp[MCPFN+1];
         void *handle;
         int (*fptr)();
         int found;

	/*=====================================================================
	 * PURPOSE: To dynamically load an external command.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kfile:  Pathname to the file containing the external command. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    index:  Index (reference) used to access this command. [i]
	 *            Used when calling zexecute to execute this command. 
	 *    nerr:   Error return flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  co/5
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MACHINE DEPENDENCIES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920409:  Added include file dload. Moved common to dload. Moved
	 *             data initialization of nfiles to initcommon block data.
	 *    900804:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900804
	 *===================================================================== */
	*nerr = 0;

	if((cmextcom.nfiles = cmextcom.nfiles + 1) > MEXTCOMS){
          *nerr = 1027;
          return;
        }

        if((env_solist = getenv("SACSOLIST")) != NULL ){
          strcpy(solist,env_solist);
	}else{
          strcpy(solist,"libsac.so");
	}


        strcpy(kfiletemp,kfile);
        funcname = strtok(kfiletemp," \t\n\0");

        soname = strtok(solist," \t\n\0");
        found = FALSE;

        while( soname != NULL ){
	  char *dumbError ;
          if((handle = dlopen(soname, RTLD_LAZY)) != NULL){
            if((fptr = (int(*)())dlsym(handle,funcname)) != NULL){
              cmextcom.extfuncs[cmextcom.nfiles-1] = fptr;
              found = TRUE;
              break;
	    }
          }          
	  dumbError = dlerror() ;

          soname = strtok(NULL," \t\n\0");
	}
        
        if( found ){
	  *index = cmextcom.nfiles;
	}else{
          *nerr = 1028;
          cmextcom.nfiles--;
          *index = -1;
	}

L_8888:
	return;

} /* end of function */

