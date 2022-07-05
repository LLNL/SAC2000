#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/dload.h"
#include "../../inc/extfunc.h"

int setup_data(sac_files **call_data, float **fyinput, float **fxinput, int* numfiles, int* nptsmax);
int setup_args(int argc, char** argv, char** fargs, int* lenarg);
int retrieve_data(sac_files **call_data, float *fyinput, float *fxinput, int numfiles, int nptsmax);

void ext_init(void)
{
        int error, value, index, ierr;
        sac_header *this_header;
 
	/*=====================================================================
	 * PURPOSE: To cause external header access routines to be loaded.
         *          This routine should never actually be called.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    none
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    none
	 *=====================================================================
	 * MODULE/LEVEL:  co/5
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MACHINE DEPENDENCIES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *   
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */

        this_header = makehdr( NULL );

        getehdr( *this_header, " ", &error );
        setehdr( *this_header, " ", 1, &error );
        getfhdr( *this_header, " ", &error );
        setfhdr( *this_header, " ", 1.0, &error );
        getnhdr( *this_header, " ", &error );
        setnhdr( *this_header, " ", 1, &error );
        getlhdr( *this_header, " ", &error );
        setlhdr( *this_header, " ", 1, &error);
        getahdr( *this_header, " ", &error );
        setahdr( *this_header, " ", " ", &error );

        index = 1;

        fgetahdr_(&index, " ", (char *)&value, &error, 1, 4);
        fgetehdr_(&index, " ", &value, &error, 1);
        fgetfhdr_(&index, " ", (float *)&value, &error, 1); 
        fgetlhdr_(&index, " ", &value, &error, 1);
        fgetnhdr_(&index, " ", &value, &error, 1);
        fsetahdr_(&index, " ", " ", &error, 1, 1);
        fsetehdr_(&index, " ", &value, &error, 1);
        fsetfhdr_(&index, " ", (float *)&value, &error, 1);
        fsetlhdr_(&index, " ", &value, &error, 1);
        fsetnhdr_(&index, " ", &value, &error, 1);

        ierr = setup_data( NULL, NULL, NULL, NULL, NULL);
        ierr = setup_args(0, NULL, NULL, NULL);
        ierr = retrieve_data(NULL, NULL, NULL, 0, 0);
 
	return;

} /* end of function */

