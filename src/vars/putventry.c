#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ putventry(vars, vars_s, name, name_s, type, length, 
	 index, nerr)
char *vars;   int vars_s;
char *name;   int name_s;
int *type, length, *index, *nerr;
{
	int roflag;
	int _l0, copyfrom, copyto, difflength, nchars, newlength, 
	 nilindex, niloffset, numcopy, varindex, varlength;
	float badindirectflag;
	void encodevdesc(), zputc();

        float *Sacmem;

	/*=====================================================================
	 * PURPOSE:  To put a vars entry into a vars list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    vars:       Name of vars list. [c]
	 *    name:       Name of vars entry. [c]
	 *    type:       Type of vars entry. [i]
	 *    length:     Number of input values. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    index:      Index in sacmem array to start storage of values. [i]
	 *    nerr:       Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    valuetype, valuelength, indirect, descindex,
	 *             desclength, namelength, varsnode, varsnilindex,
	 *             BADVALUETYPE, BADINDIRECTFLAG, VARSINCR, BIGVALUE
	 *    mem:     isacmem, sacmem
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  existsv, copy, encodevnil
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    880302:  Added varsnilindex array to store current nil index.
	 *    880208:  Modified the NIL terminator logic to include a name of
	 *             "NIL" and a length equal to that of the rest of vars list.
	 *    871013:  Deleted recursion by adding gotos back to top.
	 *             Added parameter VARSINCR.
	 *    870522:  Deleted logic that squeezed vars list when the
	 *             new value length was less than the old length.  Now
	 *             treat this the same as when the new length is larger.
	 *    861229:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861229
	 *===================================================================== */
	/* PROCEDURE: */
L_1000:
	*nerr = 0;

	/* - If variable exists and is the right type: */

	if( existsv( vars,vars_s, name,name_s ) ){
		if( cmvars.valuetype != *type ){
			*nerr = BADVALUETYPE;
			setmsg( "ERROR", *nerr );
			apcmsg( vars,vars_s );
			apcmsg( name,name_s );
			}
		else if( cmvars.indirectflag ){
			*nerr = badindirectflag;
			setmsg( "ERROR", *nerr );
			apcmsg( vars,vars_s );
			apcmsg( name,name_s );
			}
		else{
			difflength = length - cmvars.valuelength;

			/* -- If lengths are the same, copy the new values over the old ones. */
			if( difflength == 0 ){
				*index = cmvars.descindex + cmvars.desclength + cmvars.namelength;

				/* -- If the new length is different than the old one, delete the current
				 *    copy of the variable, and append new copy to end.
				 *    This is done by looping back to the top of the routine. */
				}
			else{
				roflag = cmvars.readonlyflag;
				if( roflag ){
                                        _l0 = FALSE;
					setvreadonly( vars,vars_s, name,name_s, &_l0, nerr );
				      }
				if( *nerr != 0 )
					goto L_8888;

#ifdef DEBUG
      malloc_verify();
#endif
				deletev( vars,vars_s, name,name_s, nerr );

#ifdef DEBUG
      malloc_verify();
#endif
				if( *nerr != 0 )
					goto L_8888;
				goto L_1000;
				}
			}

		/* -- If vars does not exist and there is room, append it to end. */
		}
	else{
		fstrncpy( kmvars.valuename, MAXCVNAME, name, strlen(name));
		nchars = min( indexb( kmvars.valuename,MAXCVNAME+1 ), MAXCVNAME );
		cmvars.namelength = (nchars - 1)/4 + 1;
		cmvars.valuelength = length;
		cmvars.desclength = 1;
		if( cmvars.valuelength > BIGVALUE )
			cmvars.desclength = 2;
		varlength = cmvars.desclength + cmvars.namelength + cmvars.valuelength;
		cmvars.deleteflag = FALSE;
		cmvars.readonlyflag = FALSE;
		cmvars.indirectflag = FALSE;

		/* -- Set index of new vars entry to the current nil terminator index
		 *    and calculate the new nil terminator index value. */
		varindex = Varsnilindex[cmvars.varsnode];
		nilindex = varindex + varlength;

		if( (nilindex + 1) < (Varsindex[cmvars.varsnode] + Varslength[cmvars.varsnode]) ){
                        Sacmem = cmmem.sacmem[Varsindex[cmvars.varsnode]]+(Varsnilindex[cmvars.varsnode] -
                                                                           Varsindex[cmvars.varsnode]);

#ifdef DEBUG
      malloc_verify();
#endif
			encodevdesc( Sacmem, &cmvars.deleteflag, &cmvars.readonlyflag, 
			 &cmvars.indirectflag, &cmvars.sharedflag, &cmvars.reservedflag, 
			 &cmvars.applflag1, &cmvars.applflag2, type, &cmvars.desclength, 
			 &cmvars.namelength, &cmvars.valuelength );

#ifdef DEBUG
      malloc_verify();
#endif

                        Sacmem += cmvars.desclength;
			zputc( kmvars.valuename,MAXCVNAME+1, Sacmem, 4*cmvars.namelength );
			*index = varindex + cmvars.desclength + cmvars.namelength;

#ifdef DEBUG
      malloc_verify();
#endif

			/* -- Add new NIL terminator. */
			niloffset = Varsnilindex[cmvars.varsnode] - Varsindex[cmvars.varsnode] + 
			 cmvars.desclength + cmvars.namelength + cmvars.valuelength;

#ifdef DEBUG
      malloc_verify();
#endif
			encodevnil( cmvars.varsnode, niloffset, nerr );
			if( *nerr != 0 )
				goto L_8888;

#ifdef DEBUG
      malloc_verify();
#endif

			/* -- Increase the size of the vars list if necessary and append
			 *    variable to end by going back to the top of this subroutine. */
			}
		else{
			newlength = Varslength[cmvars.varsnode] + VARSINCR*((varlength - 
			 1.0)/VARSINCR + 1.0);
#ifdef DEBUG
      malloc_verify();
#endif

			increasenlist( cmvars.varsnode, newlength, nerr );

#ifdef DEBUG
      malloc_verify();
#endif
			if( *nerr != 0 )
				goto L_8888;
			goto L_1000;
			}
		}

	/* - If no error occurred mark vars list for later saving on disk. */

L_8888:
	if( *nerr == 0 )
		Varsmodified[cmvars.varsnode] = TRUE;
	return;

} /* end of function */








