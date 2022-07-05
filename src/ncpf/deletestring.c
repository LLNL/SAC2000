#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ deletestring(string, string_s, input, input_s, output, 
	 output_s)
char *string;   int string_s;
char *input;   int input_s;
char *output;   int output_s;
{
	int index, ninput, noutput, nstring;
        char *s1;

	/*=====================================================================
	 * PURPOSE:  To delete a character string in a text string.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    string:  String to delete. [c]
	 *    input:   Input text string. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    output:  Output text string. [c]
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881228
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of string and input text. */
	nstring = (string_s - 1);
	ninput = (input_s - 1);

	/* - Locate string in input text. */

	index = indexs( input, ninput, string, nstring, TRUE, TRUE );

	/* - If string was found in text: */
	if( index > 0 ){

		/* -- Copy text (if any) that occurs before string. */
		if( index > 1 ) {
                        strncpy((s1=malloc(index)),input,index-1);    
                        s1[index-1] = '\0';
			subscpy( output, 0, index - 2, output_s - 1, s1 );
                        free(s1);
		      }
		noutput = index;

		/* -- Copy text (if any) that occurs after string.
		 *    Also fill output text with spaces. */

                strncpy((s1=malloc(input_s - (index + nstring) + 1)),input+index + 
		 nstring - 1,input_s - (index + nstring));
                s1[input_s - (index + nstring)] = '\0';

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

