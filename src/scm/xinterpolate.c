#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/scm.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ xinterpolate(nerr)
int *nerr;
{
	int j, j_, jdfl, jdfl_, ndecr,
	 ndxx, ndxy, newlen, newndx, nincr, nlen, nout;
	float xnew, xstart, xstop;



	/*=====================================================================
	 * PURPOSE: To parse and execute the action command INTERPOLATE.
	 *          This command interpolates data to new sampling rate.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  SCM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NDFL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    SCM:     DTNEW, EPS, LBREQ, BREQ, LNREQ, NREQ
	 *    MEM:     SACMEM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920227:  Added DATA-SET update logic.
	 *    871023:  Modified logic for computing interpolation window.
	 *    871012:  Added BEGIN and NPTS options.
	 *    861129:  Added ability to interpolate unevenly spaced data.
	 *    841217:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  871012
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "DELTA v":  set desired sampling interval. */
		if( lkreal( "DELTA$",7, &cmscm.dtnew ) ){

			/* -- "EPSILON v":  set interpolation error factor. */
			}
		else if( lkreal( "EPSILON$",9, &cmscm.eps ) ){

			/* -- "BEGIN ON|OFF|v": set begin time option. */
			}
		else if( lklogr( "BEGIN$",7, &cmscm.lbreq, &cmscm.breq ) ){

			/* -- "NPTS ON|OFF|v": set number of points option. */
			}
		else if( lklogi( "NPTS",5, &cmscm.lnreq, &cmscm.nreq ) ){

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;

		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
		goto L_8888;

	/* CHECKING PHASE: */

	/* - Test for a non-null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* EXECUTION PHASE: */

	/* - Perform the requested function on each file in DFL. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		jdfl_ = jdfl - 1;

		/* -- Get next file from the memory manager.
		 *    (Header is moved into common blocks CMHDR and KMHDR.) */
		getfil( jdfl, TRUE, &nlen, &ndxy, &ndxx, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Force begin time if requested. */
		if( cmscm.lbreq ){
			xstart = cmscm.breq;
			if( xstart < *b ){
				nincr = (int)( (*b - xstart)/cmscm.dtnew ) + 1;
				xstart = xstart + (float)( nincr )*cmscm.dtnew;
				setmsg( "WARNING", 2008 );
				wrtmsg( MUNOUT );
				clrmsg();
				}
			}
		else{
			xstart = *b;
			}

		/* -- Determine length of interpolated array and allocate block. */
		if( cmscm.lnreq ){
			newlen = cmscm.nreq;
			xstop = xstart + (float)( newlen - 1 )*cmscm.dtnew;
			if( xstop > *e ){
				ndecr = (int)( (xstop - *e)/cmscm.dtnew ) + 1;
				newlen = newlen - ndecr;
				xstop = xstop - (float)( ndecr )*cmscm.dtnew;
				setmsg( "WARNING", 2009 );
				wrtmsg( MUNOUT );
				clrmsg();
				}
			}
		else if( *leven ){
			newlen = (int)( *delta*((float)( *npts )/cmscm.dtnew) );
			xstop = xstart + (float)( newlen - 1 )*cmscm.dtnew;
			if( xstop > *e ){
				newlen = newlen - 1;
				xstop = xstop - cmscm.dtnew;
				}
			}
		else{
			newlen = (int)( (*e - xstart)/cmscm.dtnew );
			xstop = *e;
			}
		allamb( &cmmem, newlen, &newndx, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Perform the specific operation on this data file. */
		nout = 0;
		xnew = xstart;
		for( j = 0; j <= (newlen - 1); j++ ){
			j_ = j - 1;
			if( xnew >= *b && xnew <= *e ){
				if( *leven ){
					wigint( (float*)b, cmmem.sacmem[ndxy], nlen, *delta, 
					 cmscm.eps, xnew, cmmem.sacmem[newndx] + j );
					}
				else{
					wigint( cmmem.sacmem[ndxx], cmmem.sacmem[ndxy], nlen, 0.0, 
					 cmscm.eps, xnew, cmmem.sacmem[newndx] + j );
					}
				}
			else{
				*(cmmem.sacmem[newndx] + j) = 0.;
				nout = nout + 1;
				}
			xnew = xnew + cmscm.dtnew;
			}

		/* -- Send out error message if some points were outside range of file. */
		if( nout > 0 ){
			*nerr = 2005;
			setmsg( "ERROR", *nerr );
			apimsg( nout );
			}

		/* -- Update any header fields that may have changed. */
		*npts = newlen;
		*delta = cmscm.dtnew;
		*b = xstart;
		*e = *b + (float)( *npts - 1 )**delta;
		extrma( cmmem.sacmem[newndx], 1, newlen, depmin, depmax, depmen );

		/* -- Switch memory blocks so this file now points to the new
		 *    (interpolated) array and release the old (orginal) block. */
		cmdfm.ndxdta[jdfl_][0] = newndx;
		Nlndta[jdfl] = newlen;
		relamb( cmmem.sacmem, ndxy, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- If data was unevenly spaced, release x block and adjust header. */
		if( !*leven ){
			*leven = TRUE;
			Ncomp[jdfl] = 1;
			relamb( cmmem.sacmem, ndxx, nerr );
			if( *nerr != 0 )
				goto L_8888;
			}

		/* -- Return file to memory manager. */
		putfil( jdfl, nerr );
		if( *nerr != 0 )
			goto L_8888;

		}

	/* - Calculate and set new range of dependent variable. */

	setrng();

L_8888:
	return;

} /* end of function */

