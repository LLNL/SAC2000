#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ changestring(string1, string1_s, string2, string2_s, 
	 input, input_s, output, output_s)
char *string1;   int string1_s;
char *string2;   int string2_s;
char *input;   int input_s;
char *output;   int output_s;
{
	int index, ninput, noutput, nstring1, nstring2;
        char *s1;

	/*=====================================================================
	 * PURPOSE:  To change one character string to another in a text string.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    string1:  String to search for in input. [c]
	 *    string1:  String to substitute for in input. [c]
	 *    input:    Input text string. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    output:   Output text string. [c]
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881228
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of strings and input text. */
	nstring1 = (string1_s - 1);
	nstring2 = (string2_s - 1);
	ninput = (input_s - 1);

	/* - Locate first string in input text. */

	index = indexs( input, ninput, string1, nstring1, TRUE, TRUE );

	/* - If first string was found in text: */
	if( index > 0 ){

		/* -- Copy text (if any) that occurs before first string. */

		if( index > 1 ) {
                        strncpy((s1=malloc(index)),input,index-1);
                        s1[index-1] = '\0';
			subscpy( output, 0, index - 2, output_s - 1, s1);
                        free(s1);
		      }
		noutput = index;

		/* -- Insert second string. */

                strncpy((s1=malloc(nstring2+1)),string2,nstring2);
                s1[nstring2] = '\0';
		subscpy( output, noutput - 1, noutput + nstring2 - 2, output_s - 
		 1, s1 );
                free(s1);
		noutput = noutput + nstring2;

		/* -- Copy text (if any) that occurs after first string.
		 *    Also fill output text with spaces. */

                strncpy((s1=malloc(input_s-(index+nstring1) + 1)),input+index+nstring1-1,
                                      input_s-(index+nstring1));
                s1[input_s-(index+nstring1)] = '\0';
		subscpy( output, noutput - 1, -1, output_s - 1, s1 );
                free(s1);

		/* - If first string was not found, simply copy input text to output text. */

		}
	else{
		fstrncpy( output, output_s-1, input, strlen(input));

		}

L_8888:
	return;

} /* end of function */

