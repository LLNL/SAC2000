#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
void /*FUNCTION*/ formmarker(time, type, type_s, output, output_s, 
	 lok)
double time;
char *type;   int type_s;
char *output;   int output_s;
int *lok;
{
	int nc1, nc2;
        char *cattemp;


	/*=====================================================================
	 * PURPOSE:  To format a "time marker".
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    time:    The time of the marker. [f]
	 *    type:    The type of marker. [c]      
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    output:  The formatted output. [c]
	 *    lok:     Set to .TRUE. if value is defined, .FALSE. otherwise [l]
	 *=====================================================================
	 * MODULE/LEVEL:  LHF/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     FUNDEF, KUNDEF
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870305:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  870305
	 *===================================================================== */
	/* PROCEDURE: */
	if( time != cmhdr.fundef ){
                sprintf(output,"%16.5g",time);
		ljust( output,output_s );
		*lok = TRUE;
		nc1 = indexb( output,output_s );
		if( memcmp(type,kmhdr.kundef,min(strlen(type),strlen(kmhdr.kundef))) != 0 ){
			nc2 = indexb( type,type_s );
                        cattemp = malloc(2+nc2+2);
                        strcpy(cattemp," (");
                        strncat(cattemp,type,nc2);
                        strcat(cattemp,")");
			subscpy( output, nc1, -1, output_s - 1, cattemp );
                        free(cattemp);
			}
		else{
			subscpy( output, nc1, -1, output_s - 1, " " );
			}
		}
	else{
                fstrncpy(output, output_s-1, "UNDEFINED", 9); 
		*lok = FALSE;
		}

L_8888:

	return;

} /* end of function */

