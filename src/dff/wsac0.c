#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "hdr.h"
void /*FUNCTION*/ wsac0(kname1, xarray, yarray, nerr, kname_s)
char *kname1;   int kname_s;
float *xarray, *yarray;
int *nerr;
{
	int ncerr, nderr, nlcdsk, nun;
	float temp[MKMHDR], temp2[FILEMKMHDR];
	void zputc(), zwabs();
	char kname0[500], *kname;
	
	int i=0;
	while (*kname1 != '\0')
		kname0[i++]=*kname1++;
	kname0[i++]='\0';
	
	kname=&kname0[0];

	/*=====================================================================
	 * PURPOSE: To write a SAC file to disk using current header values.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *     KNAME:  Name of disk file to write. [c]
	 *             The name should be blank filled.
	 *    XARRAY:  Array containing independent variable. [fa]
	 *             This is not used if the data is evenly spaced.
	 *    YARRAY:  Array containing dependent variable. [fa]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MUNOUT
	 *    HDR:     FHDR
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ZGTFUN, ZNFILE, ZWABS, ZCLOSE, ZPUTC, WRTMSG
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    TEMP:    Array used while writing header character data. [fa]
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Create the requested file and open it. */

	zdest( kname,kname_s, &nderr );
	znfile( &nun, kname,kname_s, "DATA",5, nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Write the header to disk starting at word 0. */

	nlcdsk = 0;
	zwabs( &nun, cmhdr.fhdr, MCMHDR, &nlcdsk, nerr );
	if( *nerr != 0 )
	    goto L_8888;
	nlcdsk = nlcdsk + MCMHDR;
	zputc( kmhdr.khdr,9, temp, (MCPW+1)*MKHDR );           
        
        map_chdr_out(temp,temp2);

	zwabs( &nun, temp2, FILEMKMHDR, &nlcdsk, nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Write the array containing the dependent variable to disk
	 *   starting after the end of the header. */

	nlcdsk = nlcdsk + FILEMKMHDR;
	zwabs( &nun, yarray, *npts, &nlcdsk, nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - If the data is not evenly spaced, write the array
	 *   containing the independent variable. */

	if( !*leven ){
	    nlcdsk = nlcdsk + *npts;
	    zwabs( &nun, xarray, *npts, &nlcdsk, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	/* - Write any error message to terminal, close the disk file, and return. */

L_8888:
	if( *nerr != 0 )
	    wrtmsg( MUNOUT );
	zclose( &nun, &ncerr );
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    961031:  ninf and nhst were changed to norid and nevid for
	 *             compatability with the CSS format.  maf 961031
	 *    870513:  Changed call to wrtxtd to wrtmsg.
	 *    840118:  Deleted call to ZTRUNC.
	 *    830125:  Changes due to modified header common block.
	 *    820118:  Added logic to truncate file before closing.
	 *    810120:  Changed to output message retrieval from disk.
	 *    800821:  Original version [Prime].
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850307
	 *===================================================================== */

} /* end of function */




/* Wrapper to make the function more convenient for FORTRAN programmers. */

void /*FUNCTION*/ wsac0_ (kname1, xarray, yarray, nerr, kname_s)
char *kname1;   int kname_s;
float *xarray, *yarray;
int *nerr;
{
	wsac0 ( kname1 , xarray , yarray , nerr , kname_s ) ;
}
