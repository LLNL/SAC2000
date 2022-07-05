#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
void /*FUNCTION*/ getvlist2(vars, vars_s, name, name_s, mode, nerr)
char *vars;   int vars_s;
char *name;   int name_s;
char *mode;
int *nerr;
{
	char nextlistname[MAXCVNAME+1], sublistname[MAXCVNAME+1];
	int index, nc1, nc2, node;


	/*=====================================================================
	 * PURPOSE:  To get a vars entry of type LIST.
	 *           Nearly identical copy of subroutine getvlist.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:    Name of vars list. [c]
	 *    name:    Name of vars entry (i.e. vars sublist.) [c]
	 *             Set to blanks if the vars list, not a sublist is desired.
	 *    mode:    Access mode. [c]
	 *             = 'SINGLE' to get only the requested list.
	 *             = 'ALL' to get the requested list and all of its sublists.
	 *             Case insensitive and only first character is needed.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    MAXCVNAME, vlistdelim
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  existsvlist, initvlist, nextvlist, getvlist2
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    891005:  Changed recursive call to getvlist to a call to 
	 *             getvlist2 to fake out overzealous fortran compilers.
	 *    890203:  Added check for existence of vars list.
	 *    881114:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881114
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Check for existence of vars list. */

	if( !existsvlist( vars,vars_s, "BOTH", &node ) ){
		*nerr = VARSLISTNOTFOUND;
		setmsg( "ERROR", *nerr );
		apcmsg( vars,vars_s );
		goto L_8888;
		}

	/* - Create name of vars sublist.
	 *   If no sublist name was input, set sublist to list. */

	nc1 = Ncvarsname[node];
	nc2 = indexb( name,name_s );
	if( nc2 > 0 ){
                fstrncpy( sublistname, MAXCVNAME, (char *)&kmvars.vabsflag, 1);
                fstrncpy( sublistname+1, MAXCVNAME-1,
                           kmvars.varsname[node - 1], min(nc1,MAXCVNAME));
                fstrncpy( sublistname+1+min(nc1,MAXCVNAME), MAXCVNAME-1-min(nc1,MAXCVNAME),
                                            (char *)&kmvars.vlistdelim, 1);
                fstrncpy( sublistname+1+min(nc1,MAXCVNAME)+1, MAXCVNAME-1-min(nc1,MAXCVNAME)-1,
                                                                name, nc2);

		}
	else if( nc1 > 0 ){
                fstrncpy( sublistname, MAXCVNAME, (char *)&kmvars.vabsflag, 1);
                fstrncpy( sublistname+1, MAXCVNAME-1, 
                                kmvars.varsname[node - 1], min(nc1,MAXCVNAME));
		}
	else{
		fstrncpy( sublistname, MAXCVNAME, " ", 1 );
		}

	/* - Get the requested sublist. */

	initvlist( sublistname,MAXCVNAME+1, &index, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - If in 'ALL' mode, get each subsequent sublist in requested sublist.
	 *   This is done by searching requested sublist for more sublists and
	 *   recursively calling this subroutine with new request. */

	if( mode[0] == 'A' || mode[0] == 'a' ){
		cmgetvlist.nlevelsgt = cmgetvlist.nlevelsgt + 1;
		Indexsave[cmgetvlist.nlevelsgt] = index;
		strcpy( &kmgetvlist.sublistnames[cmgetvlist.nlevelsgt - 1][0], sublistname
		  );
L_1000:
		if( nextvlist( &Indexsave[cmgetvlist.nlevelsgt], nextlistname
		 ,MAXCVNAME+1 ) ){
			getvlist(kmgetvlist.sublistnames[cmgetvlist.nlevelsgt - 1][0]
			 ,MAXCENAME+1, nextlistname,MAXCVNAME+1, "ALL", nerr );
			if( *nerr != 0 )
				goto L_8888;
			goto L_1000;
			}
		else{
			cmgetvlist.nlevelsgt = cmgetvlist.nlevelsgt - 1;
			}

		/*	if indirect data block:
		 *         get data block for entry;
		 *	endif */

		}

L_8888:

	return;

} /* end of function */

