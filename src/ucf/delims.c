#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/tpf.h"
void /*FUNCTION*/ delims(ktd, ktd_s, ntd, kmd, kmd_s, nmd)
char *ktd;   int ktd_s;
int ntd;
char *kmd;   int kmd_s;
int nmd;
{
#define KTD(I_,J_)	(ktd+(I_)*(ktd_s)+(J_))
#define KMD(I_,J_)	(kmd+(I_)*(kmd_s)+(J_))
	int j, j_;



	/* Ind
	 *=====================================================================
	 * PURPOSE:  To define token and message delimiters used by TOKENS.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KTD:     Array containing token delimiters. [c]
	 *             A blank is always treated as a token delimiter unless
	 *             it is inside a message.  Multiple blanks are treated as
	 *             a single blank unless within a message.
	 *    NTD:     Length of KTD. [i]
	 *    KMD:     Array containing message delimiters. [c]
	 *    NMD:     Length of KMD. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    TPF:     MDL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    TPF:     NTOKDL, KTOKDL, NMSGDL, KMSGDL
	 *===================================================================== */
	/* PROCEDURE: */
	cmtpf.ntokdl = min( ntd, MDL );
	for( j = 1; j <= cmtpf.ntokdl; j++ ){
		j_ = j - 1;
		Ktokdl[j] = KTD(j_,0)[0];
		}

	cmtpf.nmsgdl = min( nmd, MDL );
	for( j = 1; j <= cmtpf.nmsgdl; j++ ){
		j_ = j - 1;
		Kmsgdl[j] = KMD(j_,0)[0];
		}

L_8888:
	return;
	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    810000:  Original version.
	 *===================================================================== */

#undef	KMD
#undef	KTD
} /* end of function */

