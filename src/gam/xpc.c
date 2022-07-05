#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gem.h"
#include "gam.h"

void znfiles(FILE** nfu, char *kname, int kname_s, char *ktype, int ktype_s, int* nerr);

void /*FUNCTION*/ xpc(nerr)
int *nerr;
{
	int lany, lcuron, lexist, lquit, lrplon;
	int nc;
        FILE *nunrpl;
	void zgetgd();


	/*=====================================================================
	 * PURPOSE: To parse and execute the action command PCURSOR.
	 *          This command plots objects and text under cursor control.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  gam/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    dfm:     ndfl
	 *    gam:     kgddef
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gam:
	 *    hdr:
	 *    mem:     sacmem
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    910608:  Added call to zgetgd when no graphics device specified.
	 *             Changed call to begindevice to begindevices. (wct)
	 *    880115:  Deleted forced lowercasing of pc files.
	 *    871222:  Moved file inquire to zinquire.
	 *    840627:  Added calls to save/restore plot environment.
	 *    830808:  Replaced call to ZEXIST by inquire.
	 *    821005:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while( lcmore( nerr ) ){

		/* -- "REPLAY/CREATE":  set replay file mode. */
		if( lclog2( "REPLAY$",8, "CREATE$",8, &cmgam.lrplrq ) )
		{ /* do nothing */ }

		/* -- "FILE/MACRO filename":  set type of file and filename. */
		else if( lclog2( "FILE$",6, "MACRO$",7, &cmgam.lpcfil ) ){
		    if( lcchar( MCPFN, kmgam.kpcfil,MCPFN+1, &nc ) ){
			if( cmgam.lpcfil ){
			    fstrncpy( kmgam.kpcfil, MCPFN, kmgam.kpcfil,
				      min(nc,MCPFN));
			    fstrncpy( kmgam.kpcfil+min(nc,MCPFN),
				      MCPFN-min(nc,MCPFN),
				      kmgam.kpcfsu,strlen(kmgam.kpcfsu));

			}
			else{
			    fstrncpy( kmgam.kpcfil, MCPFN, kmgam.kpcfil,
				      min(nc,MCPFN));
			    fstrncpy( kmgam.kpcfil+min(nc,MCPFN),
				      MCPFN-min(nc,MCPFN), kmgam.kpcmsu,
				      strlen(kmgam.kpcmsu));
			}
		    }
		} /* end else if( lclog2( "FILE$",6, ... */

		/* -- "BORDER [ON/OFF]":  set border option for hardcopy. */
		else if( lklog( "BORDER$",8, &cmgam.lbdrrq ) )
		{ /* do nothing */ }

		/* -- Bad syntax. */
		else{
		    cfmt( "ILLEGAL OPTION:$",17 );
		    cresp();
		}
	} /* end while( lcmore( nerr ) ) */

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */


	/* CHECKING PHASE: */

	/* - If no graphics device is on, initialize the default device. */

	getstatus( "ANY", &lany );
	if( !lany ){
	    zgetgd( kmgam.kgddef,9 );
	    begindevices( kmgam.kgddef,9, 1, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    }

	/* - Now check for a device with graphics input (cursor) capability. */

	getstatus( "CURSOR", &lcuron );

	/* - Check for replay files' existence if replay mode is requested. */

	if( cmgam.lrplrq ){
	    zinquire( kmgam.kpcfil, &lexist );
	    if( lexist ){
		lrplon = TRUE;
	    }
	    else{
		lrplon = FALSE;
		setmsg( "WARNING", 108 );
		apcmsg( kmgam.kpcfil,MCPFN+1 );
		outmsg();
	    }
	}
	else{
	    lrplon = FALSE;
	}

	/* EXECUTION PHASE: */

	/* - Save plot environment. */

	plsave();

	/* - Begin new plot frame if requested.  Also draw a border region
	 *   if to terminal or if requested to hardcopy. */

	if( cmgem.lframe ){
	    beginframe( FALSE , nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}
	getvspace( &cmgem.xvspmn, &cmgem.xvspmx, &cmgem.yvspmn, &cmgem.yvspmx );
	if( cmgem.lframe && cmgam.lbdrrq )
	    rectangle( &cmgem.xvspmn, &cmgem.xvspmx,
		       &cmgem.yvspmn, &cmgem.yvspmx );

	/* - Map unit square to the current viewspace. */

	setvport( cmgem.xvspmn, cmgem.xvspmx, cmgem.yvspmn, cmgem.yvspmx );
	setworld( 0., 1., 0., 1. );

	/* - Initialize "current data point" (CDP) to the center of the screen. */

	cmgam.xcdp = 0.5;
	cmgam.ycdp = 0.5;
	cmgam.lglori = FALSE;

	/* - Define cursor location for environment changing commands. */

	cmgam.xcdpe = 0.05;
	cmgam.ycdpe = 0.05;

	/* - If creating a new macro file, define and display macro center point. */

	if( !lrplon && !cmgam.lpcfil ){
	    cmgam.xcen = cmgam.xcdp;
	    cmgam.ycen = cmgam.ycdp;
	    worldmove( cmgam.xcen - 0.02, cmgam.ycen );
	    worlddraw( cmgam.xcen + 0.02, cmgam.ycen );
	    worldmove( cmgam.xcen, cmgam.ycen - 0.02 );
	    worlddraw( cmgam.xcen, cmgam.ycen + 0.02 );
	}

	/* - Set default text attributes. */

	cmgem.chht = cmgem.tsdef;
	cmgem.chwid = cmgem.txrat*cmgem.chht;
	settextsize( cmgem.chwid, cmgem.chht );

	/* - Open replay file. */

	znfiles( &nunrpl, kmgam.kpcfil,MCPFN+1, "TEXT",5, nerr );
	if( *nerr != 0 )
	    goto L_7777;

	/* - Execute commands from replay file if requested. */

	if( lrplon ){
	    if( cmgam.lpcfil ){
		pcxrpl( nunrpl, &lquit );
		if( lquit )
		    goto L_7777;
	    }
	    else{
		pcmrpl( nunrpl, 1., 0. );
	    }
	}

	/* - Execute commands using terminal cursor if available. */

	if( lcuron ){
	    if( cmgam.lpcfil ){
		pcxcur( nunrpl );
	    }
	    else{
		pcmcur( nunrpl );
	    }
	}

	/* - Truncate and close replay file.  End plot frame if requested.
	 *   Restore plot environment. */

L_7777:
	;
/*
Need to write routine endfil that (I think):
(see notes)
*/
/*	endfil( nunrpl );*/
/*	close_unit(nunrpl, "");*/


        fclose(nunrpl);

	plhome();
	if( cmgem.lframe )
	    endframe( nerr );
	plrest();

L_8888:

	return;

} /* end of function */

