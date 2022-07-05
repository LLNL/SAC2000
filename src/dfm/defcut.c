#include <stdio.h>
#include <stdlib.h>
#include <float.h>	/* added. maf 961211 */
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"

/* Macro to round float or double to nearest int.  maf 970214 */
#define  NINT( x ) ( (x) >= 0 ? (int) ( (x) + 0.5 ) : (int) ( (x) - 0.5 ) )
void apcmsg2(char* kalpha, int kalpha_s);
void /*FUNCTION*/ defcut( char kcut[ 2 ][ 9 ] , float ocut[ 2 ] ,
			  int idfl , int *nerr )
{
	int ic1, ic2, jdx, nptrd;
	float pick[2], start, stop = 0;

	float *const Pick = &pick[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To define cut parameters for a given data file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *     IDFL:   Data file list number. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    RNDOFF
	 *    DFM:     KCUT, OCUT, IPCKHD, KPICK, MPICK, ICUTER, KDFL
	 *    HDR:     FHDR, IPCKHD, FUNDEF, NPTS
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    DFM:     NTOTAL, NSTART, NSTOP, NFILLB, NFILLE
	 *    HDR:     BEGIN, NPTS, ENND
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  SETMSG, APCMSG, OUTMSG
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    START:   Start time in seconds for this file. [f]
	 *    STOP:    Stop time in seconds for this file. [f]
	 *    PICK:    Start and stop pick reference times. [f]
	 *    LNSTRT:  Used to flag a start cut of "N". [l]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    961211, 970114, 970214, and 970304:  
	 *             I made incremental changes to harden cut.  It now cuts
	 *             precisely on the values entered.  maf
	 *    880128:  Fixed bug that occurred when the sampling interval
	 *             was smaller than the machine roundoff factor.
	 *    850415:  Changes due to restructuring of DFM common block.
	 *    840228:  Original version from DEFMEM.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850415
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Get file name from character list. */

	lnumcl( kmdfm.kdfl,MAXCHARS, idfl, &ic1, &ic2 );

	/* - Save total number of points in file. */

	Ntotal[idfl] = *npts;

	/* - Compute start value. */

	if( strcmp(kcut[0],"Z       ") == 0 ){
		Pick[1] = 0.;
	}

	else if ( strcmp(kcut[0],"N       ") != 0 ) {
		jdx = nequal( (char*)kcut[0], (char*)kmdfm.kpick,9, MPICK );
		if( jdx > 0 )
			Pick[1] = Fhdr[Ipckhd[jdx]];

		else{
			*nerr = 901;
			setmsg( "ERROR", *nerr );
			apcmsg( "DEFCUT #2",10 );
			return ;
		}
	}

	/* - Check to make sure the requested pick field in the header is defined. */

	if( Pick[1] == cmhdr.fundef ){
		if( cmdfm.icuter == 1 ){
			*nerr = 1322;
			setmsg( "ERROR", 1322 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			return ;
		}
		else{
			setmsg( "WARNING", 1322 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			outmsg();
			setmsg( "OUTPUT", 1330 );
			outmsg();
			start = *begin;
			Nstart[idfl] = 1;
		}
	}
	else{	/* overhauled maf 961211 */ /* overhauled again.  maf 970214 */ 
		/* overhauled yet again. maf 970304 */
		int iBegin , iStart ;

		start = Pick[1] + ocut[0] ;               /* start time of data to read */
		iStart = NINT(  start / *delta ) ;
		iBegin = NINT( *begin / *delta ) ;

		Nstart[idfl] = iStart - iBegin + 1 ;
	}

	/* -  Compute stop value. */

	if( strcmp(kcut[1],"N       ") == 0 ){
		nptrd = ocut[1] + RNDOFF**delta;
		stop = start + (float)( nptrd - 1 )**delta;
		Nstop[idfl] = Nstart[idfl] + nptrd - 1;
		Pick[2] = 0.;
	}
	else if( strcmp(kcut[1],"Z       ") == 0 )
		Pick[2] = 0.;

	else{
		jdx = nequal( (char*)kcut[1], (char*)kmdfm.kpick,9, MPICK );
		if( jdx > 0 )
			Pick[2] = Fhdr[Ipckhd[jdx]];

		else{
			*nerr = 901;
			setmsg( "ERROR", *nerr );
			apcmsg( "DEFCUT #3",10 );
			return ;
		}
	}

	/* - Make sure stop pick is defined. */

	if( Pick[2] == cmhdr.fundef ){
		if( cmdfm.icuter == 1 ){
			*nerr = 1323;
			setmsg( "ERROR", *nerr );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			return ;
		}
		else{
			setmsg( "WARNING", 1323 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			outmsg();
			setmsg( "OUTPUT", 1331 );
			outmsg();
			stop = *ennd;
			Nstop[idfl] = *npts;
		}
	}
	else if( strcmp(kcut[1],"N       ") != 0 ){  /* overhauled, maf 961211 */
                int iBegin , iStop ;			  /* overhauled again. maf 970304 */

                stop = Pick[2] + ocut[1] ;               /* stop time of data to read */
                iStop = NINT(  stop / *delta ) ;
                iBegin = NINT( *begin / *delta ) ;

                Nstop[idfl] = iStop - iBegin + 1 ;
	}

	/* - Check that start value less than stop value. */

	if( start >= stop ){
		*nerr = 1328;
		setmsg( "ERROR", *nerr );
                apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
		return ;
	}

	/* - Handle cases where the requested data window is not entirely
	 *   within the range of the data file. */

	/* -- Entire data window after file end. */
	if( Nstart[idfl] > *npts ){
		if( cmdfm.icuter == 3 ){
			Nfillb[idfl] = 0;
			Nfille[idfl] = Nstop[idfl] - Nstart[idfl] + 1;
		}
		else{
			*nerr = 1326;
			setmsg( "ERROR", *nerr );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
		}
		return ;
	}

	/* -- Entire data window before file begin. */
	if( Nstop[idfl] < 1 ){
		if( cmdfm.icuter == 3 ){
			Nfillb[idfl] = Nstop[idfl] - Nstart[idfl] + 1;
			Nfille[idfl] = 0;
		}
		else{
			*nerr = 1327;
			setmsg( "ERROR", *nerr );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
		}
		return ;
	}

	/* - Start of data window before file begin. */
	if( Nstart[idfl] < 1 ){
		if( cmdfm.icuter == 3 )
			Nfillb[idfl] = 1 - Nstart[idfl];

		else if( cmdfm.icuter == 2 ){
			setmsg( "WARNING", 1324 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			outmsg();
			setmsg( "OUTPUT", 1330 );
			outmsg();
			start = *begin;
			Nstart[idfl] = 1;
			Nfillb[idfl] = 0;
		}
		else{
			*nerr = 1324;
			setmsg( "ERROR", *nerr );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			return ;
		}
	}
	else
		Nfillb[idfl] = 0;


	/* -- Stop of data window is after file end. */
	if( Nstop[idfl] > *npts ){
		if( cmdfm.icuter == 3 )
			Nfille[idfl] = Nstop[idfl] - *npts;

		else if( cmdfm.icuter == 2 ){
			setmsg( "WARNING", 1325 );
                        apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			outmsg();
			setmsg( "OUTPUT", 1331 );
			outmsg();
			stop = *ennd;
			Nstop[idfl] = *npts;
			Nfille[idfl] = 0;
		}
		else{
			*nerr = 1325;
			setmsg( "ERROR", *nerr );
			return ;
		}
	}
	else
		Nfille[idfl] = 0;


	/* - Convert these start and stop points to new begin and end times. */

	*begin = *begin + (float)( Nstart[idfl] - 1 )**delta;
	*npts = Nstop[idfl] - Nstart[idfl] + 1;
	*ennd = *begin + (float)( *npts - 1 )**delta;

} /* end of function */

