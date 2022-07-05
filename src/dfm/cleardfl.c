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
void /*FUNCTION*/ cleardfl(nerr)
int *nerr;
{
	int jdfl, jdfl_, jcomp ;


	/*=====================================================================
	 * PURPOSE:  Clears pointers in data-set storage for current data-sets.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag.  Set to zero if no error occurs.
	 *=====================================================================
	 * MODULE/LEVEL:  DSM
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:  NCDSNDX, MNDSNDX
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    DFM:     NDFL, NDSNDX
	 *    MEM:     SACMEM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INITCL
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920406:  Update count of total files in SAC memory.
	 *    911017:  Added reset to nflncds, number of files in the data-set.
	 *    910827:  Adapted for multiple data-sets.
	 *    910813:  Original version. Replaces part of the old cleardfl.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  
	 *===================================================================== */
	/* the included file hdr is only used so that global variables can
	 * be accessed in the debugger. */
	/* PROCEDURE: */
	*nerr = 0;

	/* - For each file in storage */

	for( jdfl = 1; jdfl <= cmdfm.ndsflcnt; jdfl++ ){
	    jdfl_ = jdfl - 1;

	    /* -- If the data set index matches the current data set */

	    if( Ndsndx[jdfl] == 1 ){

		/* --- Release header block. */
		if( Ndxhdr[jdfl] > 0 ){
		    relamb( cmmem.sacmem, Ndxhdr[jdfl], nerr );
		    if( *nerr != 0 )
			goto L_8888;
		    Ndxhdr[jdfl] = 0;
		}

		for( jcomp = 0; jcomp < Ncomp[jdfl]; jcomp++ ){
		    if( cmdfm.ndxdta[jdfl_][jcomp] > 0 ){
			relamb( cmmem.sacmem, cmdfm.ndxdta[jdfl_][jcomp], nerr );
			if( *nerr != 0 )
			    goto L_8888;
			cmdfm.ndxdta[jdfl_][jcomp] = 0;
		    }
		}


		/* --- Reset the data set index, # of components
		 *     and data file length  */

		Ndsndx[jdfl] = 0;
		Ncomp[jdfl] = 0;
		Nlndta[jdfl] = 0;

	    } /* end if( Ndsndx[jdfl] == 1 ) */

	} /* end for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ) */

        for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
                jdfl_ = jdfl - 1;

                /* -- Initialize SDD header block pointer. */
                if( Nxsdd[jdfl] > 0 )
                        Nxsdd[jdfl] = 0;

        } /* end for ( jdfl ) */

        /* - Clear DFL name storage. */
        memset ( kmdfm.kdfl , ' ' , MAXCHARS - 1 );

	/* - Zap the file count of files in memory. */

	cmdfm.ndsflcnt = 0 ;
	cmdfm.ndfl = 0;

L_8888:
	return;

} /* end of function */

