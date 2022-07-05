#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/mem.h"
#include "../../inc/csf.h"
void /*FUNCTION*/ initcommon()
{
	char _c0[2], _c1[2];
	int _l0, nerr;
/*	void exit();  */


	/*=====================================================================
	 * PURPOSE: Variable initialization of ALL common blocks.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    KWSNGL, KWMULT, KWCONC
	 *    MEM:     MMEM, MEPSL
	 *    CSF:     KTOKDL, NTOKDL, KMSGDL, NMSGDL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    MEM:     SACMEM
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920414:  Moved block data initialization to initsac as a new
	 *             procedure, initblkdata, while portint to IBM RISC 6000.
	 *    920410:  Added nlevelsgt data initialization after taking it out
	 *             of getvlist and getvlist2, moving common to inc/vars.
	 *
	 *    920309:  Added include dload and initialization of global nfiles.
	 *    920330:  Added block data initialization of common variables.
	 *             Added include gem, dfm, cnv, vars. Added initialization 
	 *             of ndsflcnt - files in data-set storage.
	 *    911107:  Added nerr check and exti on initcomlists.
	 *    900405:  Added call to INITCONTATTR.
	 *    900305:  Added call to INIXYZ.
	 *    880520:  Fixed bug in call to endgraphics.
	 *    870527:  Deleted call to INIWVT.
	 *    870513:  Deleted call to INIOFM.
	 *    870317:  Added call to INIICM.
	 *    870301:  Added call to create global variable store.
	 *    860407:  Added call to INIAM.
	 *    860327:  Added call to INIWVT.
	 *             Moved call to DELIMS from INISAC to here.
	 *    860218:  Added call to WILDCH.
	 *    830309:  Added call to end graphics library.
	 *    810514:  Added call to INISNF
	 *    810414:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900305
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Terminate graphics library. */
	endgraphics( &nerr );

	/* - SAC's common blocks. */

	inibom();
	inicom();
	inicsf();
	inicpf();
	inidfm();
	inieam();
	iniexm();
	inifks();
	inigem();
	inigam();
	inihdr();
	inilhf();
	inisam();
	iniscm();
	inispe();
	initpf();
	initok();
	inisss();
	iniuom();
	iniusr();
	inisnf();
	initcomlists( &nerr );
	if( nerr == 901 )
		exit(1);
	initsite();
	inismm();
	inibbs();
	iniicm();
	inivars();
	inixyz();
	initcontattr();

	/* - SACMEM array manager. */

	iniam( &cmmem );

	/* - Blackboard store. */

	createbbs( &nerr );

	/* - Command stack delimiters and special characters. */

	delims( (char*)kmcsf.ktokdl,9, cmcsf.ntokdl, (char*)kmcsf.kmsgdl
	 ,9, cmcsf.nmsgdl );

	/* - Initialize wildcard patterns with system values. */

	wildch( KWSNGL, KWMULT, KWCONC );

L_8888:
	return;

} /* end of function */

