#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/csf.h"
void /*FUNCTION*/ inicsf()
{



	/*=====================================================================
	 * PURPOSE: Variable initialization of common block CMCSF.
	 *=====================================================================
	 * PARAMETERS:
	 *=====================================================================
	 * VARIABLE DEFINITIONS:
	 *=====================================================================
	 * MODULE/LEVEL:  CSF/4
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870301:  Added percent and apersand as message delimiters.
	 *    870211:  Deleted exclamation mark as a message delimiter.
	 *    860930:  Deleted question mark as a token delimter.
	 *    820505:  Added double quote as message delimiter.
	 *    820122:  Deleted initialization of MCMSG.
	 *    810901:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	cmcsf.ncs = MCS;
	cmcsf.ncur = 1;
	cmcsf.lcsemp = TRUE;
	cmcsf.nlev = 0;
	cmcsf.ntop = 0;
	cmcsf.nmvtop = 0;
	cmcsf.nbot = 0;
	cmcsf.nmvbot = 0;
	cmcsf.narg = MARG;
	cmcsf.narg1 = 0;
	cmcsf.narg2 = 0;
	strcpy( kmcsf.kinv[0], ":I      " );
	strcpy( kmcsf.kinv[1], ":J      " );
	strcpy( kmcsf.kpass, ":P      " );
	strcpy( kmcsf.keocs, ":Z      " );
	strcpy( kmcsf.kappnd, ":A      " );
	strcpy( kmcsf.kcmt, ":C      " );
	strcpy( kmcsf.kmac[0], ":B      " );
	strcpy( kmcsf.kmac[1], ":E      " );
	strcpy( kmcsf.ksingl, ":S      " );
	strcpy( kmcsf.ktext, ":T      " );
	strcpy( kmcsf.kdel, ":D      " );
	strcpy( kmcsf.kquery, ":Q      " );
	strcpy( kmcsf.klabl, ":L      " );
	strcpy( kmcsf.ksvout[0], ":1      " );
	strcpy( kmcsf.ksvout[1], ":2      " );
	strcpy( kmcsf.ksvout[2], ":3      " );
	strcpy( kmcsf.ksvout[3], ":4      " );
	strcpy( kmcsf.ksvout[4], ":5      " );
	strcpy( kmcsf.ksvout[5], ":6      " );
	strcpy( kmcsf.ksvout[6], ":7      " );
	strcpy( kmcsf.ksvout[7], ":8      " );
	strcpy( kmcsf.ksvout[8], ":9      " );
	cmcsf.neoc = 1;
	strcpy( kmcsf.keoc[0], ";       " );
	cmcsf.ntokdl = 1;
	strcpy( kmcsf.ktokdl[0], ";       " );
	cmcsf.nmsgdl = 4;
	strcpy( kmcsf.kmsgdl[0], "'       " );
	strcpy( kmcsf.kmsgdl[1], "\"       " );
	strcpy( kmcsf.kmsgdl[2], "%       " );
	strcpy( kmcsf.kmsgdl[3], "&       " );

L_8888:
	return;

} /* end of function */

