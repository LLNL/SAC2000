#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gd2.h"
void /*FUNCTION*/ flushbuffer2(nerr)
int *nerr;
{
	char kfrn[4], knam[MCPFN+1];
	int _l0, numw;
	void zwabs();
	int *const mfbufl = (int*)cmgd2.mfbuf;


	/*=====================================================================
	 * PURPOSE:  To flush the buffer for graphics device 2 (SGF).
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  GD2/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GD2:     MOPNUL, JFBPNT, MFBUF, JFDPNT
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GD2:     JFBPNT, MFBUF, JFDPNT
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ZWABS
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861014:  Original version based on FFLUSH.
	 *=====================================================================
	 * DOCUMENTED:  861014
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - If buffer is not empty: */

	if( cmgd2.jfbpnt > 1 ){

		/* -- Force an even number of 16-bit integers in the current buffer,
		 *    by appending a NULL opcode if necessary.
		 *    This is needed because ZWABS works with int (32-bit) words. */

		if( (cmgd2.jfbpnt/2)*2 == cmgd2.jfbpnt ){
			Mfbuf[cmgd2.jfbpnt] = MOPNUL;
			}
		else{
			cmgd2.jfbpnt = cmgd2.jfbpnt - 1;
			}
		numw = cmgd2.jfbpnt/2;

		/* -- Store the size of the buffer in the first word being written.
		 *    This is a integer physically stored immediately before the short
		 *    integer buffer. This length is used to facilitate the reading process. */
		cmgd2.nfbuf = numw;

#ifdef DEBUG
                printf("writing %d words to sgf file\n",numw+1);
#endif                
		/* -- Write the buffer to disk, including the length of the buffer. */
		zwabs( &cmgd2.jfun, &cmgd2.nfbuf, (numw + 1), &cmgd2.jfdpnt, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Update the disk and buffer pointers. */
		cmgd2.jfdpnt = cmgd2.jfdpnt + numw + 1;
		cmgd2.jfbpnt = 1;
		}

L_8888:
	return;

} /* end of function */

