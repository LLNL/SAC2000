#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MARGS	10

#include "../../inc/mach.h"
#include "../../inc/cpf.h"

void apcmsg2(char* kalpha, int kalpha_s);



void /*FUNCTION*/ processstring(kfunction, kfunction_s, nc, ic, index, 
	 kvalue, kvalue_s, nerr)
char *kfunction;   int kfunction_s;
int nc, *ic, *index;
char *kvalue;   int kvalue_s;
int *nerr;
{
	char kmessage[MCMSG+1];
	int ic1, ic2, icstart[MARGS], icstop[MARGS], jargs, jargs_, 
	 n1, n2, nargs, nc1, nc2, ncmax;
	void *_p0, *_p1, *_p2, zgpmsg();
        char *s1, *s2, *s3;


	int *const Icstart = &icstart[0] - 1;
	int *const Icstop = &icstop[0] - 1;



	/*=====================================================================
	 * PURPOSE:  To "process" an inline string function from a SAC command line.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kfunction:   Body of function before expansion. [c]
	 *    nc:          Number of characters in function. [i]
	 *    ic:          Pointer to current character in function. [i]
	 *    index:       Index number of the string function to process. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    kvalue:      Value of function after expansion. [c]
	 *    nerr:        Error return flag. [i]
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getstringargs, changestring, deletestring, indexs
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *    970129:  Add parameter (0) to cnvati.  0 means that if a string
         *             of digits is too int, let it slide by.  maf 
	 *    890818:  Added REPLY function.
	 *    890112:  Changed APPEND and PREPEND to CONCATENATE.
	 *    890109:  Added SUBSTRING function.
	 *    881228:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881228
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Get the arguments for this function. */

	getstringargs( kfunction,kfunction_s, nc, ic, MARGS, icstart, 
	 icstop, &nargs, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Raise error condition if the number of arguments is incorrect. */

	if( Nstringargs[*index] >= 0 && Nstringargs[*index] != nargs ){
		*nerr = 1021;
		setmsg( "ERROR", *nerr );
		apimsg( Nstringargs[*index] );
		aplmsg( kfunction,kfunction_s );
		goto L_8888;
		}

	/* - Based on the index number, jump to the coding for this function. */

	switch( *index ){
		case 1: goto L_1010;
		case 2: goto L_1020;
		case 3: goto L_1030;
		case 4: goto L_1040;
		case 5: goto L_1050;
		case 6: goto L_1060;
		case 7: goto L_1070;
		}

	/*  (This piece of coding should never be executed.) */

	*nerr = 901;
	setmsg( "ERROR", *nerr );
	apcmsg( "in processstring:",18 );
	apimsg( *index );
	goto L_8888;

	/* -- Change "string1" to "string2" in "text".
	 *    Syntax: CHANGE string1 string2 text */

L_1010:

        strncpy((s1=malloc(Icstop[1]-Icstart[1] + 2)),kfunction+Icstart[1] - 1,Icstop[1] - 
	 Icstart[1] + 1);
        s1[Icstop[1]-Icstart[1] + 1] = '\0';
        strncpy((s2=malloc(Icstop[2] - Icstart[2] + 2)),kfunction+Icstart[2] - 
	 1,Icstop[2] - Icstart[2] + 1);
        s2[Icstop[2]-Icstart[2] + 1] = '\0';
        strncpy((s3=malloc(Icstop[3] - Icstart[3] + 2)),kfunction+Icstart[3] - 
	 1,Icstop[3] - Icstart[3] + 1);
        s3[Icstop[3]-Icstart[3] + 1] = '\0';

	changestring( s1, Icstop[1] - Icstart[1] + 2, s2, Icstop[2] - Icstart[2] + 2,
                      s3, Icstop[3] - Icstart[3] + 2, kvalue, kvalue_s );

	 free(s1);
         free(s2);
         free(s3);

	goto L_8888;

	/* -- Delete "string" in "text".
	 *    Syntax: DELETE string text */

L_1020:

        strncpy((s1=malloc(Icstop[1]-Icstart[1] + 2)),kfunction+Icstart[1] - 1,Icstop[1] - 
	 Icstart[1] + 1);
        s1[Icstop[1]-Icstart[1] + 1] = '\0';

        strncpy((s2=malloc(Icstop[2] - Icstart[2] + 2)),kfunction+Icstart[2] - 
	 1,Icstop[2] - Icstart[2] + 1);
        s2[Icstop[2] - Icstart[2] + 1] = '\0';

	deletestring( s1, Icstop[1] - Icstart[1] + 2, 
                      s2, Icstop[2] - Icstart[2] + 2, kvalue, kvalue_s );

        free(s1);
        free(s2);
	goto L_8888;

	/* -- Return portion of "text" that occurs before "string".
	 *    SYNTAX: BEFORE string text */

L_1030:
	nc1 = Icstop[1] - Icstart[1] + 1;
	nc2 = Icstop[2] - Icstart[2] + 1;

        strncpy((s1=malloc(Icstop[2]-Icstart[2]+2)),
                 kfunction+Icstart[2] - 1,Icstop[2]-Icstart[2]+1);
        s1[Icstop[2]-Icstart[2]+1] = '\0';

        strncpy((s2=malloc(Icstop[1]-Icstart[1]+2)),
                  kfunction+Icstart[1] - 1,Icstop[1]-Icstart[1]+1);
        s2[Icstop[1]-Icstart[1]+1] = '\0';

	*index = indexs( s1, nc2, s2, nc1, TRUE, TRUE );

        free(s1);
        free(s2);

	if( *index > 0 ){
		fstrncpy( kvalue, kvalue_s-1, kfunction+Icstart[2] - 1,Icstart[2] + 
		 *index - Icstart[2] - 1);
		}
	else{
		fstrncpy( kvalue, kvalue_s-1, " ", 1 );
		}
	goto L_8888;

	/* -- Return portion of "text" that occurs after "string".
	 *    Syntax: AFTER string text */

L_1040:
	nc1 = Icstop[1] - Icstart[1] + 1;
	nc2 = Icstop[2] - Icstart[2] + 1;

        strncpy((s1=malloc(Icstop[2]-Icstart[2]+2)),
                 kfunction+Icstart[2] - 1,Icstop[2]-Icstart[2]+1);
        s1[Icstop[2]-Icstart[2]+1] = '\0';

        strncpy((s2=malloc(Icstop[1]-Icstart[1]+2)),
                 kfunction+Icstart[1] - 1,Icstop[1]-Icstart[1]+1);
        s2[Icstop[1]-Icstart[1]+1] = '\0';

	*index = indexs( s1, nc2, s2, nc1, TRUE, TRUE );

        free(s1);
        free(s2);

	if( *index > 0 ){
		fstrncpy( kvalue, kvalue_s-1, kfunction+Icstart[2] + *index + nc1 - 
		 2,Icstop[2] - (Icstart[2] + *index + nc1 - 1) + 1);
		}
	else{
		fstrncpy( kvalue, kvalue_s-1, " ", 1 );
		}
	goto L_8888;

	/* -- Return characters "n1" through "n2" of "text".
	 *    Syntax: SUBSTRING n1 n2 text */

L_1050:

        strncpy((s1=malloc(Icstop[1]-Icstart[1]+2)),
                 kfunction+Icstart[1] - 1,Icstop[1]-Icstart[1]+1);
        s1[Icstop[1]-Icstart[1]+1] = '\0';

	cnvati( s1, Icstop[1] - Icstart[1] + 2, &n1, 0, nerr );
						/* add 0. maf 970129 */
	
        free(s1);

	if( *nerr == 0 ){
		n1 = max( n1, 1 );
		ic1 = Icstart[3] + n1 - 1;
		}
	else{
		*nerr = 1025;
		setmsg( "ERROR", *nerr );
                apcmsg2(&kfunction[Icstart[1] - 1],Icstop[1] - Icstart[1] + 1);
		aplmsg( kfunction,kfunction_s );
		fstrncpy( kvalue, kvalue_s-1, " ", 1 );
		goto L_8888;
		}
	if( memcmp(kfunction+Icstart[2] - 1,"end",3) == 0 ||
            memcmp(kfunction+Icstart[2] - 1,"END",3) == 0 ){
		ic2 = Icstop[3];
		}
	else{
                strncpy((s1=malloc(Icstop[2]-Icstart[2]+2)),
                         kfunction+Icstart[2] - 1,Icstop[2]-Icstart[2]+1);
                s1[Icstop[2]-Icstart[2]+1] = '\0';

		cnvati( s1, Icstop[2] - Icstart[2] + 2, &n2, 0, nerr );
							/* add 0. maf 970129 */

                free(s1);

		if( *nerr == 0 ){
			ic2 = Icstart[3] + n2 - 1;
			}
		else{
			*nerr = 1025;
			setmsg( "ERROR", *nerr );
                        apcmsg2(&kfunction[Icstart[1] - 1],Icstop[1] - Icstart[1] + 1);
			aplmsg( kfunction,kfunction_s );
			fstrncpy( kvalue, kvalue_s-1, " ", 1 );
			goto L_8888;
			}
		}
	fstrncpy( kvalue, kvalue_s-1, kfunction+ic1 - 1,ic2 - ic1 + 1);

	goto L_8888;

	/* -- Concatenate one or more "strings" together.
	 *    Syntax: CONCATENATE string1 string2 ... */

L_1060:
	fstrncpy( kvalue, kvalue_s-1, " ", 1 );
	ncmax = (kvalue_s - 1);
	nc1 = 1;
	for( jargs = 1; jargs <= nargs; jargs++ ){
		jargs_ = jargs - 1;
		nc2 = min( ncmax, nc1 + Icstop[jargs] - Icstart[jargs] );

                strncpy((s1=malloc(Icstop[jargs] - Icstart[jargs] + 2)),
                     kfunction+Icstart[jargs]-1,Icstop[jargs] - Icstart[jargs] + 1);
                s1[Icstop[jargs] - Icstart[jargs] + 1] = '\0';

		subscpy( kvalue, nc1 - 1, nc2 - 1, kvalue_s - 1, s1 );

                free(s1);

		nc1 = nc2 + 1;
		if( nc1 > ncmax )
			goto L_8888;
		}
	goto L_8888;

	/* -- Send prompt to terminal and get a reply.
	 *    Syntax: REPLY string */

L_1070:
        fstrncpy( kmessage, MCMSG, kfunction+Icstart[1] - 
	 1,Icstop[1] - Icstart[1] + 1);
        *(kmessage+(Icstop[1] - Icstart[1] + 1)) = '$';

	zgpmsg( kmessage,MCMSG+1, kvalue,kvalue_s );

	/* The following if statement added to permit
	   default values in replies.  maf 961114 */
	if ( indexa ( kvalue , kvalue_s , ' ' , TRUE , FALSE ) == 0 ) {
	    char * start, * end ;

	    start = strchr ( kmessage , '[' ) ;
	    end   = strchr ( kmessage , ']' ) ;

	    if ( start != NULL && end != NULL && end > start && end - kmessage <= MCMSG ) {
		strncpy ( kvalue , start + 1 , end - start - 1 ) ;
		kvalue [ end - start - 1 ] = '\0' ;
	    }
	}

	goto L_8888;

L_8888:

	return;

} /* end of function */

