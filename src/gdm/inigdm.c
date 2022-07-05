#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gdm.h"

void loadctable(char* name, char* directory, int* nentry, int* nerr);

void /*FUNCTION*/ inigdm(nerr)
int *nerr;
{
	int lthard;
	int j, j_, jgd, jgd_, nc, nentry;
	float offset;
	void getdeviceinfo3(), getdeviceinfo4(), initdevice3(), initdevice4();
        void getdeviceinfo5(), initdevice5();


	/*=====================================================================
	 * *** INTERNAL SUBROUTINE:  NOT NORMALLY CALLED BY USER ***
	 *=====================================================================
	 * PURPOSE:  To initialize the Graphics Device Module.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lginit
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     ALL.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  endgraphics, initdevice1, initdevice2, initdevice3,
	 *             initdevice4, getdeviceinfo1, getdeviceinfo2,
	 *             getdeviceinfo3, getdeviceinfo4, calstatus, 
	 *             initctable, setcolor
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920330:  Block data initialization of lginit moved to initsac.
	 *    900803:  Added initialization of color table and initial color.
	 *    870501:  Added initialization of viewspace clipping flag.
	 *    870416:  Added call to calstatus.
	 *    861010:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870416
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - If library is already initialized, terminate it gracefully. */

	if( cmgdm.lginit ){
		endgraphics( nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	/* - Initialize common block. */

	/* -- Device flags. */
	for( jgd = 1; jgd <= MGD; jgd++ ){
		jgd_ = jgd - 1;
		Lgdon[jgd] = FALSE;
		Iflcur[jgd] = 0;
		Iflhc[jgd] = 0;
		}

	cmgdm.igdcur = 3;
	cmgdm.igdhc = 2;
	cmgdm.igdtxt = 3;

	/* -- Begin frame flag. */
	cmgdm.lbegf = FALSE;

	/* -- Window attributes. */
	for( j = 1; j <= MWINDOWS; j++ ){
		j_ = j - 1;
		offset = 0.02*(float)( j - 1 );
		Xwindowmin[j] = 0.05 + offset;
		Xwindowmax[j] = 0.85 + offset;
		Ywindowmin[j] = 0.40 + offset;
		Ywindowmax[j] = 0.95 + offset;
		}
	cmgdm.iwindow = 1;

	/* -- Viewspace attributes. */
	cmgdm.lvsful = TRUE;
	cmgdm.vsrat = 0.75;
	cmgdm.lvsclip = TRUE;

	/* -- Text attributes. */
	cmgdm.ltsoft = TRUE;
	lthard = FALSE;
	cmgdm.thgt = 0.02;
	cmgdm.twidth = 0.667*cmgdm.thgt;
	cmgdm.tangle = 0.0;

	/* -- Software font attributes. */
	strcpy( kmgdm.kgtfn[0], "simplx                                  " );
	strcpy( kmgdm.kgtfn[1], "duplex                                  " );
	strcpy( kmgdm.kgtfn[2], "complx                                  " );
	strcpy( kmgdm.kgtfn[3], "triplx                                  " );
	cmgdm.nfont = -1;
	settextfont( 1 );

	/* -- Line drawing attributes. */
	cmgdm.iline = 1;

	/* - Default color table and initial color value. */

	initctable( "default",8, &nentry, nerr );
	if( *nerr != 0 )
		goto L_8888;
	/*      call setcolor(nentry-1) */

        loadctable("color.tbl1.sac",NULL,&cmgdm.npscimage,nerr);
        if( *nerr != 0 ){
        /* color table not found--use the data loaded one */
          *nerr = 0;
          cmgdm.npscimage = NPSCIMAGE;
	}
	/* - Initialize each device's common blocks. */

/*	initdevice1();	*/
	initdevice2();
	initdevice3();
	initdevice4();
        /* initdevice5 gets called from gsacinit */

	/* - Get the names of graphics devices. */

	getdeviceinfo1( (char*)kmgdm.kgdnam[0],13, &Igdtyp[1] );
	getdeviceinfo2( (char*)kmgdm.kgdnam[1],13, &Igdtyp[2] );
	getdeviceinfo3( kmgdm.kgdnam[2],13, &Igdtyp[3] );
	getdeviceinfo4( kmgdm.kgdnam[3],13, &Igdtyp[4] );
	if( !cmgdm.lgui ) strcpy(kmgdm.kgdnam[4],"            ");

        /* getdeviceinfo5( kmgdm.kgdnam[4],13, &Igdtyp[5] );  called from */
        /*           gsacinit                                             */          

	/* - Blank fill end of graphics device names. */

	for( j = 1; j <= MGD; j++ ){
		j_ = j - 1;
		nc = indexb( (char*)kmgdm.kgdnam[j_],13 );
		subscpy( kmgdm.kgdnam[j_], nc, -1, 12, "            " );
		}

	/* - Set the "library initialized" flag. */

	cmgdm.lginit = TRUE;

	/* - Calculate graphics status variables. */

	calstatus();

L_8888:
	return;

} /* end of function */

