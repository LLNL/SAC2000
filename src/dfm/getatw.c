#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
void apcmsg2(char* kalpha, int kalpha_s);

void /*FUNCTION*/ getatw(krtw, krtw_s, ortw, tmin, tmax, nofmin, nlnwin, nerr)
char *krtw;   int krtw_s;
float ortw[], *tmin, *tmax;
int *nofmin, *nlnwin, *nerr;
{
#define KRTW(I_,J_)	(krtw+(I_)*(krtw_s)+(J_))
	int ic1, ic2, irtb, irte, nofmax, num;
	float rtrb, rtre ;

	float *const Ortw = &ortw[0] - 1;


	/*=====================================================================
	 * PURPOSE: To convert a "relative time window" to an "absolute time
	 *          window" by examining the header for the current data file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KRTW:    Two element array containing starting and ending
	 *             "relative time picks". [k]
	 *    ORTW:    Two element array containing starting and ending
	 *             "relative time offsets". [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    TMIN:    Minimum value of absolute time window. [f]
	 *    TMAX:    Maximum value of absolute time window. [f]
	 *    NOFMIN:  Window offset in points of minimum. [i]
	 *    NLNWIN:  Number of points in window. [i]
	 *    NERR:    Error return flag.  Set to zero if no error occurred.
	 *             Possible error numbers:  1313, 1322, 1323
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     KPICK, MPICK, IPCKN, IPCKG
	 *    HDR:     IPCKHD, FHDR, FUNDEF
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  NEQUAL, GTOUTM
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *     860304:  Added calculation of NOFWIN and NLNWIN.
	 *     820623:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861128
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Get indexes of start and stop time picks. */

	irtb = nequal( KRTW(0,0), (char*)kmdfm.kpick,9, MPICK );
	if( (irtb <= 0 || irtb == cmdfm.ipckn) || irtb == cmdfm.ipckg ){
		*nerr = 1313;
		setmsg( "ERROR", *nerr );
		goto L_8888;
	}
	irte = nequal( KRTW(1,0), (char*)kmdfm.kpick,9, MPICK );
	if( irte <= 0 || irte == cmdfm.ipckg ){
		*nerr = 1313;
		setmsg( "ERROR", *nerr );
		goto L_8888;
	}

	/* - Determine start absolute time window. */

	if( irtb == cmdfm.ipckz ){
		*tmin = Ortw[1];
	}
	else{
		rtrb = Fhdr[Ipckhd[irtb]];
		if( rtrb != cmhdr.fundef ){
			*tmin = rtrb + Ortw[1];
		}
		else{
			*tmin = *begin;
			*nerr = 1322;
			setmsg( "ERROR", *nerr );
			lnumcl( kmdfm.kdfl,MAXCHARS, cmdfm.idflc, &ic1, &ic2 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			goto L_8888;
		}
	}

	/* - Determine stop absolute window. */

	if( irte == cmdfm.ipckz ){
		*tmax = Ortw[2];
	}
	else if( irte == cmdfm.ipckn ){
		num = (int)( Ortw[2] );
		*tmax = *tmin + *delta*(float)( num );
	}
	else{
		rtre = Fhdr[Ipckhd[irte]];
		if( rtre != cmhdr.fundef ){
			*tmax = rtre + Ortw[2];
		}
		else{
			*tmax = *ennd;
			*nerr = 1323;
			setmsg( "ERROR", *nerr );
			lnumcl( kmdfm.kdfl,MAXCHARS, cmdfm.idflc, &ic1, &ic2 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			goto L_8888;
		}
	}

	/* - Determine offset and length of window in points. */

	*nofmin = (int)( (*tmin - *begin)/ *delta );
	nofmax = (int)( (*tmax - *begin)/ *delta );
	*nlnwin = nofmax - *nofmin + 1;

L_8888:
	return;

#undef	KRTW
} /* end of function */

