#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/exm.h"
void /*FUNCTION*/ setprompt(prompt, prompt_s)
char *prompt;   int prompt_s;
{
	int nc;

	/*=====================================================================
	 * PURPOSE:  To set the prompt that is sent to the user.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    PROMPT:  Prompt to send to user. [c]
	 *             There is a maximum of 10 characters in the prompt.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    EXM:     KPRMT
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861203:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  861203
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine number of characters in prompt, excluding trailing blanks. */
	nc = min( indexb( prompt,prompt_s ), 10 );

	/* - Append a " $" to end of prompt and store in EXM common block.
	 *   (The space is output, the dollar sign terminates the string.) */

        fstrncpy( kmexm.kprmt, 12, prompt, nc);
        fstrncpy( kmexm.kprmt+nc, 12-nc, " $", 2);

L_8888:

	return;

} /* end of function */

