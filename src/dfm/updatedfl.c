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
#include "../../inc/dload.h"
#include "../../inc/extfunc.h"

void updatedfl(sac_files call_data, int update, int* nerr)
{
        char kline[MCMSG+1], kfile[MCPFN+1];
        int jdfl, ndxh, ndx1, ndx2, i, ndflsave;
        sac_header *this_header;
        float *ydata, *xdata;
        
	/*=====================================================================
	 * PURPOSE: To replace or append to filelist in memory
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    call_data:  Struct containing descriptions of sac files. (see 
         *                                                 extfunc.h )
         *    update   :  Flag specifying whether to replace files in memory or
         *                to add more files to the list.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:   Error return flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MACHINE DEPENDENCIES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    960229:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900804
	 *===================================================================== */
	*nerr = 0;
 
        if( update == REPLACE ) {
	    cleardfl(nerr);
	    if( *nerr != 0 ) return;
	}

        /* check to make sure that there is room for the files. */ 
        if((cmdfm.ndfl + call_data.nfiles) > MDFL ){
	    setmsg("OUTPUT", 0);
	    sprintf(kline,"%s%3d%s","Adding ", call_data.nfiles ,
              " files would exceed the maximum number of files SAC can handle.");
	    aplmsg(kline,MCMSG+1);
	    aplmsg("No update being done.");
	    wrtmsg( MUNOUT );
	    clrmsg();
	    *nerr = 6010;
	    return;
	}

        ndflsave = cmdfm.ndfl;
        cmdfm.ndfl += call_data.nfiles;

        for( i=0; i<call_data.nfiles; i++ ){
	    this_header = call_data.ext_hdrs[i];
	    ydata       = call_data.ext_yvalues[i];
	    xdata       = call_data.ext_xvalues[i];

	    jdfl = ndflsave + i + 1 ;
	    if( getlhdr(this_header, "leven", nerr) == TRUE ){ /* if evenly spaced */
		Ncomp[jdfl] = 1;
	    }else{
		Ncomp[jdfl] = 2;
	    }          
	    Nlndta[jdfl] = getnhdr(this_header, "npts", nerr);
	    Ndsndx[jdfl] = 1 ;
          
	    if( i <= 9 ){
		sprintf(kfile,"%s%1d", "EXTERN0", i );
	    }else{
		sprintf(kfile,"%s%2d", "EXTERN",  i );
	    }

	    /* filename to storage */
	    putcl( kmdfm.kdfl, MAXCHARS, kfile, MCPFN+1, nerr);
	    if( *nerr != 0 ) return;

	    /* allocate space for a sac file in memory */
	    crsac( jdfl, Ncomp[jdfl], Nlndta[jdfl], &ndxh,
              &ndx1, &ndx2, nerr);
	    if( *nerr != 0 ) return;

	    /* store the header away */
	    memcpy(cmhdr.fhdr, this_header->ext_fhdr, MFHDR*sizeof(float));
	    memcpy(cmhdr.nhdr, this_header->ext_nhdr, MNHDR*sizeof(int));
	    memcpy(cmhdr.ihdr, this_header->ext_ihdr, MIHDR*sizeof(int));
	    memcpy(cmhdr.lhdr, this_header->ext_lhdr, MLHDR*sizeof(int));
	    memcpy(kmhdr.khdr, this_header->ext_khdr, MKHDR*9);

	    /* store the data */
	    memcpy(cmmem.sacmem[ndx1], ydata, (Nlndta[jdfl]*sizeof(float)));
	    if(Ncomp[jdfl] == 2)
		memcpy(cmmem.sacmem[ndx2], xdata, (Nlndta[jdfl]*sizeof(float)));

	    extrma( cmmem.sacmem[ndx1], 1, *npts, depmin, depmax, depmen);

	    putfil( jdfl, nerr);
	    if( *nerr != 0 ) return;
	}

	return;

} /* end of function */

