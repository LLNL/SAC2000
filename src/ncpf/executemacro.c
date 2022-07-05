#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MCMACROLINE	1000

#include "../../inc/mach.h"

void apcmsg2(char* kalpha, int kalpha_s);


void /*FUNCTION*/ executemacro(kmacroname, kmacroname_s, kmacroargs, 
	 kmacroargs_s, nerr)
char *kmacroname;   int kmacroname_s;
char *kmacroargs;   int kmacroargs_s;
int *nerr;
{
	char kmacroline[1001];
	int idx , ncerr, ncmacroline;
	static int imacrolevel = 0;

	/*=====================================================================
	 * PURPOSE: To execute a SAC macro (command) file.
	 *=====================================================================
	 * INTPUT ARGUMENTS:
	 *    kmacroname:   Name of SAC macro. [c]
	 *    kmacroargs:   Text of arguments to macro. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     openmacro, macroline, macrostatus, closemacro,
	 *             setmsg, apcmsg, apimsg, sacccommands
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900129:  Changed arguments to openmacro, macroline, and closemacro.
	 *    871109:  Increased size of "kline".
	 *    870721:  Added storage of macro names by level number.
	 *             Added termination of macro logic upon execution error.
	 *    870416:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870416
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	for( idx = 0 ; idx < 1000 ; idx++ )
	    kmacroline[ idx ] = ' ' ;
	kmacroline[ 1000 ] = '\0' ;

	/* - Increment and set the macro nesting level counter.
	 *   Initialize status indicator. */

	imacrolevel = imacrolevel + 1;
	setmacrostatus( "OK",3 );
	setmacrolevel( imacrolevel );

	/* - Open macro file. */

	openmacro( kmacroname,kmacroname_s, kmacroargs,kmacroargs_s, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Get each line from macro file and execute it. */

L_1000:
	if( macroline( kmacroline,1000, &ncmacroline, nerr ) ){
		if( *nerr != 0 )
			goto L_7000;
		if( ncmacroline > 0 )
			saccommands( kmacroline,1000, nerr );
		if( *nerr != 0 || !macrostatus() ){
			*nerr = 1016;
			setmsg( "ERROR", *nerr );
			apcmsg( kmacroname,kmacroname_s );
			aplmsg( "Command line is:",17 );
                        apcmsg2(kmacroline,ncmacroline);
			goto L_7000;
			}
		goto L_1000;
		}

	/* - Close macro file and decrement macro level counter. */

L_7000:
	closemacro( &ncerr );
	imacrolevel = imacrolevel - 1;
	setmacrolevel( imacrolevel );

L_8888:
	return;

} /* end of function */

