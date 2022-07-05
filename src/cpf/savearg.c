#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
void /*FUNCTION*/ savearg(argno,arg,nchar)
int argno;
char *arg;
int nchar;
{

        char **tempptr;
        int i;

	/*=====================================================================
	 * PURPOSE: To save the original command line arguments.
	 *=====================================================================
	 * MODULE/LEVEL:  COM/4
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * PARAMETERS:
	 *=====================================================================
	 * VARIABLE DEFINITIONS:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *
	 *    940719:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */

        if(kmcom.nkargs == 0){
         /* allocate the initial kargs pointers */
          if( (kmcom.kargs = (char **)malloc(MKARGS*sizeof(char *))) == NULL){
            printf("Error allocating initial argument storage--savearg\n quitting\n");
            exit(1);
          }
          kmcom.nkargs_allocated = MKARGS;
          for (i=0; i<MKARGS; i++) kmcom.kargs[i] = NULL; 
	}


        kmcom.nkargs = argno;

        if(kmcom.nkargs == 1)      /* release args from last call */
          for (i=0; i<kmcom.nkargs; i++) {
             free(kmcom.kargs[i]);
             kmcom.kargs[i] = NULL;
	   }

        if(kmcom.nkargs >= kmcom.nkargs_allocated){
         /* increase size of kargs array */
          if((tempptr = (char **)realloc(kmcom.kargs,(kmcom.nkargs_allocated+MKARGS)
                                    *sizeof(char *))) == NULL){
            printf("Error increasing size of argument storage array--savearg\n quitting\n");
            exit(1);
	  }else{
            kmcom.kargs = tempptr;
            for (i=kmcom.nkargs_allocated; i<(kmcom.nkargs_allocated+MKARGS); i++) kmcom.kargs[i] = NULL;
            kmcom.nkargs_allocated += MKARGS;
	  }
	}

        kmcom.kargs[kmcom.nkargs-1] = malloc(nchar+1);
        strncpy(kmcom.kargs[kmcom.nkargs-1],arg,nchar);
        *(kmcom.kargs[kmcom.nkargs-1]+nchar) = '\0';
        

	return;

} /* end of function */

