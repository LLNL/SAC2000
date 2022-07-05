#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#define	MMSGDL	2

#include "../../inc/mach.h"
#include "../../inc/cpf.h"
void /*FUNCTION*/ processfunction(kfunction, kfunction_s, kvalue, 
	 kvalue_s, nerr)
char *kfunction;   int kfunction_s;
char *kvalue;   int kvalue_s;
int *nerr;
{
        char *kname;
	int iabbrev, ic, ic1, ic2, index, itype, nc, ncmod;
	float value;
	static byte kmsgdl[MMSGDL]={'"','\''};
        char *s1;


	byte *const Kmsgdl = &kmsgdl[0] - 1;


	/*====================================================================
	 * PURPOSE:  To "process" an inline function from a SAC command line.
	 *====================================================================
	 * INPUT ARGUMENTS:
	 *    kfunction:   Body of function before expansion. [c]
	 *====================================================================
	 * OUTPUT ARGUMENTS:
	 *    kvalue:      Value of function after expansion. [c]
	 *    nerr:        Error return flag. [i]
	 *====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  tokdel, indexb, poptok, changestring, appendstring, 
	 *             prependstring, indexs
	 *====================================================================
	 * LOCAL VARIABLES:
	 *====================================================================
	 * MODIFICATION HISTORY:
         *    970129:  Add parameter (0) to cnvatf.  0 means that if a string
         *             of digits is too int, let it slide by.  maf 
	 *    920325:  Kludged in call to process "gettime max|min value" .
	 *    890922:  Limited test on "name" of function to MCPW characters.
	 *    881228:  Original version.
	 *====================================================================
	 * DOCUMENTED/REVIEWED:  881228
	 *====================================================================*/
	/* PROCEDURE: */
	*nerr = 0;

	/* - Set up message delimiters. */

	tokdel( " ",2, 0, kmsgdl,1, MMSGDL );

	/* - Determine length of function and initialize counters. */

	nc = indexb( kfunction,kfunction_s );
	ic = 0;

	/* - Pop first token from function.  This is it's "name".
	 *   Convert to uppercase and copy to a local variable. */

	poptok( kfunction, nc, &ic, &ic1, &ic2, &itype );
	ncmod = ic2 - ic1 + 1;

        strncpy((s1=malloc(ic2-ic1+2)),kfunction+ic1 - 1,ic2 - ic1 + 1);
        s1[ic2 - ic1 + 1] = '\0';

        kname = malloc(ic2-ic1+2);
        memset(kname,' ',ic2-ic1+1);
        kname[ic2-ic1+1] = '\0';

	modcase( TRUE, s1, ncmod, kname );

        free(s1);

	/* - Evaluate function name. */

	/*  There are three major types:
	 *  (1) Numeric functions which begin with the function name.
	 *      Some of these numeric functions also have abbreviated names.
	 *      Special numeric functions "gettime" and "getepoch" are handled 
	 *      separately.
	 *  (2) Character string manipulation which begin with the function name
	 *  (3) Embedded numeric which begin with a number and have
	 *      the name of the function embedded in the argument list. */

	if( lequal ( kname,ic2-ic1+2 , (char*) kmcpf.knumericfuncs,9 ,
	  cmcpf.nnumericfuncs , &index ) ) {
		/* - Inline GETTIME function (index 21) has keyword and
		     optional numeric value. */
		if( index == 21 )
			gettime( kfunction, &nc, ic, kvalue,kvalue_s, nerr );

		else if ( index == 22 )
			getepoch( kfunction, &nc, ic, kvalue, nerr ) ;

		else
			processnumeric( kfunction,kfunction_s, nc, &ic, index, 
			 kvalue,kvalue_s, nerr );

	} /* end kmcpf.knumericfuncs */

	else if ( lequal ( kname,ic2-ic1+2 , (char*) kmcpf.knumericabbrevs,9 ,
	  cmcpf.nnumericabbrevs , &iabbrev ) ) {
		index = Inumericabbrevs[iabbrev];
		processnumeric( kfunction,kfunction_s, nc, &ic, index, 
		  kvalue,kvalue_s, nerr );

	} /* end kmcpf.knumericabbrevs */

	else if ( lequal ( kname,ic2-ic1+2 , (char*) kmcpf.kstringfuncs,9 ,
	  cmcpf.nstringfuncs , &index ) ) {
		processstring( kfunction,kfunction_s, nc, &ic, &index,
		 kvalue,kvalue_s, nerr );

	} /* end kmcpf.kstringfuncs */

	else{
		cnvatf( kname,ic2-ic1+2, &value, 0, nerr ); /* add 0 before nerr
							       maf 970129 */
		if( *nerr == 0 ){
			processembedded( kfunction,kfunction_s, nc, &ic, value, 
			 kvalue,kvalue_s, nerr );
			if( *nerr != 0 )
				goto L_8888;
		}
		else{
			*nerr = 1020;
			setmsg( "ERROR", *nerr );
			apcmsg( kname,ic2-ic1+2 );
			goto L_8888;
		}
	}

	/* - Unset message delimiters before returning. */

L_8888:
        free(kname);
	tokdel( " ",2, 0, " ",2, 0 );
	return;

} /* end of function */

