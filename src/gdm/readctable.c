#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ readctable(name, name_s, max_, red, green, blue, 
	 cnames, cnames_s, nentry, nerr)
char *name;   int name_s;
int max_;
float red[], green[], blue[];
char *cnames;   int cnames_s;
int *nentry, *nerr;
{
#define CNAMES(I_,J_)	(cnames+(I_)*(cnames_s)+(J_))
	char ctable[MCPFN+1], line[MCMSG+1];
	int  idx ;
	int ic, ic1, ic2, itype, nc, numsave;
        FILE *nun;
	float bluev, greenv, redv;
	void zbasename();
        char *s1;

        for( idx = 0 ; idx < MCPFN ; idx++ )
            ctable[ idx ] = ' ' ;
        ctable[ MCPFN ] = '\0' ;

	/*=====================================================================
	 * *** INTERNAL SUBROUTINE:  NOT NORMALLY CALLED BY USER ***
	 *=====================================================================
	 * PURPOSE:  To set the color table by name.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    name:    Name of color table to set. [c]
	 *             = 'default' for default color table.
	 *             = 'grays' for a gray-scale table.
	 *             = 'rainbow' for an interesting color table.
	 *    max:     Maximum length of color arrays passed. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    red:     Array of red values. [ra]
	 *    green:   Array of green values. [ra]
	 *    blue:    Array of blue values. [ra]
	 *    cnames:  Array of color names. [ca]
	 *    nentry:  Number of entries in the color table. [i]
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    mcpfn, mcmsg
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  crname, zgtfun, zopen, poptok, cnvatf, zclose, setmsg, apcmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *    970129:  Add parameter (0) to cnvatf.  0 means that if a string
         *             of digits is too long, let it slide by.  maf 
	 *    870923:  Deleted ".saf" from aux file names.
	 *    861020:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861020
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Build name of color table file. */
	zbasename( ctable,MCPFN+1 );
	crname( ctable,MCPFN+1, KSUBDL, "ctables",8, nerr );
	if( *nerr != 0 )
		goto L_8888;
	crname( ctable,MCPFN+1, KDIRDL, name,name_s, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Open color table file. */

	zopens( &nun, ctable,MCPFN+1, "TEXT",5, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Initialize entry number counter. */

	*nentry = 0;

	/* - Read each line from disk file. */

L_1000:
        if(fgets( line,MCMSG+1,nun)==NULL) {
          if(feof(nun)) goto L_5000;
          goto L_9000;
        }
        if(line[(numsave=strlen(line)-1)] == '\n') line[numsave] = ' ';

	nc = numsave + 1;;
	ic = 0;

	/* -- Pop first three tokens.  They are the red, green, and blue values. */
	poptok( line, nc, &ic, &ic1, &ic2, &itype );
	if( itype == 1 ){
                strncpy((s1=malloc(ic2-ic1+2)),line+ic1 - 1,ic2-ic1+1);
                s1[ic2-ic1+1] = '\0';
		cnvatf( s1, ic2-ic1 + 2, &redv, 0, nerr ); /* add 0 before nerr. maf 970129 */
		free(s1);
		}
	else{
		*nerr = 2201;
		setmsg( "ERROR", *nerr );
		aplmsg( line, MCMSG+1 );
		}
	if( *nerr != 0 )
		goto L_8888;

	poptok( line, nc, &ic, &ic1, &ic2, &itype );
	if( itype == 1 ){
                strncpy((s1=malloc(ic2-ic1+2)),line+ic1 - 1,ic2-ic1+1);
                s1[ic2-ic1+1] = '\0';
		cnvatf( s1, ic2-ic1 + 2, &greenv, 0, nerr ); /* add 0 before nerr. maf 970129 */
		free(s1);
		}
	else{
		*nerr = 2201;
		setmsg( "ERROR", *nerr );
		aplmsg( line, nc);
		}
	if( *nerr != 0 )
		goto L_8888;

	poptok( line, nc, &ic, &ic1, &ic2, &itype );
	if( itype == 1 ){
                strncpy((s1=malloc(ic2-ic1+2)),line+ic1 - 1,ic2-ic1+1);
                s1[ic2-ic1+1] = '\0';
		cnvatf( s1, ic2- ic1 + 2, &bluev, 0, nerr ); /* add 0 before nerr. maf 970129 */
		free(s1);
		}
	else{
		*nerr = 2201;
		setmsg( "ERROR", *nerr );
		aplmsg( line, nc);
		}
	if( *nerr != 0 )
		goto L_8888;

	/* -- Store values in color table if everything is okay. */
	red[*nentry] = redv;
	green[*nentry] = greenv;
	blue[*nentry] = bluev;

	/* -- Pop next token.  It is an optional color name. */
	poptok( line, nc, &ic, &ic1, &ic2, &itype );
	if( itype == 1 ){
		fstrncpy( CNAMES(*nentry,0), cnames_s-1, line+ic1 - 1,min(ic2,MCMSG) - 
		 ic1 + 1);
		}
	else{
		fstrncpy( CNAMES(*nentry,0), cnames_s-1, "UNKNOWN", 7 );
		}

	/* -- Increment color table entry pointer and loop until end-of-file */
	*nentry = *nentry + 1;
	goto L_1000;

	/* - Come to here on end-of-file */

L_5000:
	;

L_8888:
	zcloses( &nun, nerr );
	return;

	/* - Come to here on errors during read. */

L_9000:
	*nerr = 114;
	setmsg( "ERROR", *nerr );
	apcmsg( ctable,MCPFN+1 );
	goto L_8888;

#undef	CNAMES
} /* end of function */

