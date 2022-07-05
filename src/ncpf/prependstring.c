#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ prependstring(string, string_s, input, input_s, 
	 output, output_s)
char *string;   int string_s;
char *input;   int input_s;
char *output;   int output_s;
{
	int ninput, nstring;


	/*=====================================================================
	 * PURPOSE:  To prepend a character string to a text string.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    string:   String to prepend to input text. [c]
	 *    input:    Input text string. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    output:   Output text string. [c]
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881228
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of string and input text. */
	nstring = (string_s - 1);
	ninput = (input_s - 1);

	/* - Concatenate string and input text to form output text.
	 *   Also fill output text with spaces. */

        fstrncpy( output, output_s-1, string, nstring);
        fstrncpy( output+nstring, output_s-1-nstring, input, ninput);

L_8888:

	return;

} /* end of function */

