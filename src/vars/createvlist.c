#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ createvlist(fullvars, fullvars_s, length, node, 
	 nerr)
char *fullvars;   int fullvars_s;
int length, *node, *nerr;
{
	char ktemp[MAXCVNAME+1];
	int _l0, icchild1, icchild2, icparent2, index, nchars, niloffset, 
	 type;
	void encodevdesc(), zputc();


	/*=====================================================================
	 * PURPOSE:  To create a NEW vars list in memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    fullvars:  Name of new vars list to create. [c]
	 *               This is the full vars list name, including the
	 *               directory pathname.  It is not expanded using the
	 *               "current vars list" like most of the vars subroutines.
	 *    length:    Length of vars list. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    node:    The vars list node number. [i]
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * SPECIAL NOTES:
	 * - You should use this subroutine to create new TOP LEVEL vars list.
	 * - You should use "putvlist" to create new vars sublists within an
	 *   existing vars list.
	 * - The directory pathname is necessary if you plan to save the list on
	 *   disk.  It is not needed for temporary vars lists used for scratch space.
	 * - This subroutine is also called by other vars subroutines when
	 *   reading existing vars lists from disk to memory.
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    KSUBDL
	 *    vars:    MAXCVNAME, MAXVARS, MAXVARSEXCEEDED, VALUELIST, 
	 *             vlistdelim, BIGVALUE
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    varsname, varsindex
	 *    mem:     sacmem
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  allamb, indexb, encodevdesc, zputc, encodevnil
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890227:  Added output of vars node number.
	 *    881205:  Moved encoding of nil descriptor to new subroutine.
	 *    881114:  Changed name from createvsect to createvlist.
	 *    870915:  First entry in vars list changed to name of vars
	 *             list.  Before all of them were called "vars".
	 *    861229:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861229
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Allocate space for vars list, store name, and index. */

	allamb( &cmmem, length, &index, nerr );
	if( *nerr != 0 )
		goto L_8888;

        /* - Zero out the allocated memory */
        memset(cmmem.sacmem[index],'\0',length*sizeof(float));

	allocatevnode( node, nerr );
	if( *nerr != 0 )
		goto L_8888;
	if( fullvars[0] == kmvars.vabsflag ){
		fstrncpy( kmvars.varsname[*node - 1], MAXCVNAME, fullvars+1,fullvars_s - 
		 2);
		}
	else{
		fstrncpy( kmvars.varsname[*node - 1], MAXCVNAME, fullvars, fullvars_s - 
		 1);
		}
	strcpy( ktemp, kmvars.varsname[*node - 1] );
	Ncvarsname[*node] = indexb( (char*)kmvars.varsname[*node - 1],MAXCVNAME+1 );
	Varsindex[*node] = index;
	Varslength[*node] = length;
	Varsmodified[*node] = TRUE;
	Varsindirect[*node] = FALSE;

	/* - Initialize vars list. */

	/* -- Determine the last sublist name of the vars list name. */
	splitvname( (char*)kmvars.varsname[*node - 1],MAXCVNAME+1, Ncvarsname[*node], 
	 &icparent2, &icchild1 );
	icchild2 = Ncvarsname[*node];

	/* -- Encode vars list data type with length of entire list. */
	type = VALUELIST;
	fstrncpy( kmvars.valuename, MAXCVNAME, kmvars.varsname[*node - 1]+icchild1 - 
	 1,min(icchild2,MAXCVNAME) - icchild1 + 1);
	strcpy( ktemp, kmvars.valuename );
	cmvars.deleteflag = FALSE;
	cmvars.readonlyflag = FALSE;
	cmvars.indirectflag = FALSE;
	cmvars.sharedflag = FALSE;
	cmvars.reservedflag = FALSE;
	cmvars.applflag1 = FALSE;
	cmvars.applflag2 = FALSE;
	cmvars.desclength = 1;
	nchars = icchild2 - icchild1 + 1;
	cmvars.namelength = (nchars - 1)/4 + 1;
	cmvars.valuelength = length - cmvars.desclength - cmvars.namelength;
	if( cmvars.valuelength > BIGVALUE ){
		cmvars.desclength = 2;
		cmvars.valuelength = cmvars.valuelength - 1;
		}
	encodevdesc( cmmem.sacmem[index], &cmvars.deleteflag, &cmvars.readonlyflag, 
	 &cmvars.indirectflag, &cmvars.sharedflag, &cmvars.reservedflag, 
	 &cmvars.applflag1, &cmvars.applflag2, &type, &cmvars.desclength, 
	 &cmvars.namelength, &cmvars.valuelength );
	index = index + cmvars.desclength;
	zputc( kmvars.valuename,MAXCVNAME+1, cmmem.sacmem[Varsindex[*node]]+cmvars.desclength,
                                                                 4*cmvars.namelength );

	/* -- Append vars list terminator to create an empty list. */
	niloffset = cmvars.desclength + cmvars.namelength;
	encodevnil( *node, niloffset, nerr );
	if( *nerr != 0 )
		goto L_8888;

L_8888:
	return;

} /* end of function */

