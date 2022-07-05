#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void getfil(int idfl, int ldta, int* nlen, int* ndx1, int* ndx2, int* nerr)
{
	char kfile[MCPFN+1];
	int ndxh, nlcmem;
	void zgetc();



	/*=====================================================================
	 * PURPOSE: To get data file from memory manager.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IDFL:    Data file list index number. [i]
	 *    LDTA:    Set to .TRUE. if header and data is desired.
	 *             Set to .FALSE. if only header is desired. [l]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NLEN:    Number of data points in file. [i]
	 *    NDX1:    Index in SACMEM array of first data component. [i]
	 *    NDX2:    Index in SACMEM array of second component, if any. [i]
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1310, 1335
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCPW
	 *    DFM:     NDFL, NDXHDR, NLNDTA, NDXDTA, NCOMP
	 *    HDR:     MCMHDR, MKHDR
	 *    MEM:     SACMEM
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    HDR:     All
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  COPY, ZPUTC, GTOUTM, RDSAC
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900322:  Changed value of ndx2 from 0 to 1 when there is no second
	 *             data component. (VAX/VMS bug fix.)
	 *    850801:  Changes in argument list for RDSAC.
	 *    850415:  Changes due to restructuring of DFM common block.
	 *    810923:  Added error return when data was requested
	 *             and only the headers were read.
	 *    810120:  Changed to output message retrieval from disk.
	 *    790606:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850415
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - If legitimate data file index number: */

	if( idfl > 0 && idfl <= cmdfm.ndfl ){

		/* -- If in memory mode, move header to HDR common and get data indexes. */
		nlcmem = Ndxhdr[idfl];
		copy( (int*)cmmem.sacmem[nlcmem], (int*)&Fhdr[1], MCMHDR );
		zgetc( cmmem.sacmem[nlcmem] + MCMHDR, kmhdr.khdr[0], (MCPW+1)* MKHDR );
		if( ldta ){
			*nlen = Nlndta[idfl];
			if( *nlen <= 0 ){
				*nerr = 1335;
				setmsg( "ERROR", *nerr );
				*ndx1 = 0;
				*ndx2 = 0;
				goto L_8888;
			}
			*ndx1 = cmdfm.ndxdta[idfl - 1][0];
			if( Ncomp[idfl] > 1 ){
				*ndx2 = cmdfm.ndxdta[idfl - 1][1];
			}
			else{
				*ndx2 = 1;
			}
		}
		cmdfm.idflc = idfl;
	}
	else{
		*nerr = 1310;
		setmsg( "ERROR", *nerr );
		apimsg( idfl );
		*ndx1 = 0;
		*ndx2 = 0;
		*nlen = 0;
	}
L_8888:

	return;

} /* end of function */

