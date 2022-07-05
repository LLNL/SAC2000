#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ gennames(headerfield, lenheaderfield, filelist, lenfilelist, nfiles, nerr)
char *headerfield;
int lenheaderfield;
char *filelist;
int lenfilelist;
int nfiles;
int *nerr;
{
	int ic1, ic2, jdfl, jdfl_, ndx1, ndx2, nlen;
	void *_p0;
        char nameout[41];
        char procname[41];
        char *field;
        int flength;

	/*=====================================================================
	 * PURPOSE:  To generate a list of filenames based upon the contents
	 *           of the input header field.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag.  Set to zero if no error occurs.
	 *        
	 *        
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NDFL
	 *    HDR:     IFTYPE, ITIME, IXY, LEVEN
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

        memset(filelist,' ',lenfilelist);
        filelist[lenfilelist] = '\0';

	/* - For each file in DFL: */

	for( jdfl = 1; jdfl <= nfiles; jdfl++ ){
		jdfl_ = jdfl - 1;

		/* -- Get header from memory manager. */
		getfil( jdfl, FALSE, &nlen, &ndx1, &ndx2, nerr );
		if( *nerr != 0 )
			goto L_8888;

                strcpy(nameout,"                                        ");
                flength = 0;

                formhv(headerfield,lenheaderfield,3,nameout,41,nerr);
                if(*nerr != 0) goto L_8888;

                /* replace interior blanks with a . */
                if((field = strtok(nameout," ")) == NULL){
                  *nerr = 901;
                  goto L_8888;
		}

                strcpy(procname,field);
                flength += strlen(field);
                while((field = strtok(NULL," ")) != NULL){
                  procname[flength] = '.';
                  strcpy(procname+flength+1,field);
                  flength += strlen(field) + 1; 
		}

                procname[flength] = ' ';
                procname[flength+1] = '\0';

                flength += 1;
   
                putcl(filelist,lenfilelist,procname,flength,nerr);
                if(*nerr != 0)goto L_8888;                
		}

        makeuniq(filelist,lenfilelist,nfiles,nerr);

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    850322:  Fixed logic in checking for spectral files.
	 *    820622:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850322
	 *===================================================================== */

} /* end of function */

