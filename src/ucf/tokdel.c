#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/tok.h"
void /*FUNCTION*/ tokdel(ktd, ktd_s, ntd, kmd, kmd_s, nmd)
char *ktd;   int ktd_s;
int ntd;
char *kmd;   int kmd_s;
int nmd;
{
#define KTD(I_,J_)	(ktd+(I_)*(ktd_s)+(J_))
#define KMD(I_,J_)	(kmd+(I_)*(kmd_s)+(J_))
	int j, j_;


	/*=====================================================================
	 * PURPOSE: To define token and message delimiters for POPTOK.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KTD:     Token delimiters [char*1 array].
	 *    NTD:     Length of KTD [max=5].
	 *    KMD:     Message delimiters [char*1 array].
	 *    NMD:     Length of MTD [max=5].
	 *=====================================================================
	 * MODULE/LEVEL:
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    TOK:     MTOKDL, MMSGDL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    TOK:     KTOKDL, NTOKDL, KMSGDL, NMSGDL
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Store symbol delimiters into common variables. */
	cmtok.ntokdl = min( ntd, MTOKDL );
	for( j = 1; j <= cmtok.ntokdl; j++ ){
		j_ = j - 1;
/*		Ktokdl[j] = KTD(j_,0)[0]; */
		kmtok.ktokdl[j_] = KTD(j_,0)[0];
		}

	/* - Store message delimiters into common variables. */
	cmtok.nmsgdl = min( nmd, MMSGDL );
	for( j = 1; j <= cmtok.nmsgdl; j++ ){
		j_ = j - 1;
/*		Kmsgdl[j] = KMD(j_,0)[0]; */
		kmtok.kmsgdl[j_] = KMD(j_,0)[0];
		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820419:  Original version.
	 *===================================================================== */

#undef	KMD
#undef	KTD
} /* end of function */

