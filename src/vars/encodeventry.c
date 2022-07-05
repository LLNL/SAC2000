#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/vars.h"
#include "../../inc/nvars.h"
#include "../../inc/mem.h"
int /*FUNCTION*/ encodeventry(index, name, name_s, value, value_s)
int *index;
char *name;   int name_s;
char *value;   int value_s;
{
	char dtemp[25], temp[17];
	int done, encodeventry_v;
	int _l0, jvalue, nc, ncv1, ncv2, ncvmax, offset;
	float singletemp;
	double doubletemp;
	void decodevdesc(), zgetc();
        char *s1;

        float *Sacmem;

	/*=====================================================================
	 * PURPOSE:  To encode (format) the current vars entry in a vars list.
	 *           Used for sequential access of a vars list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    index:   The index of the current entry. [i]
	 *             Defined by an initial call to "initvlist".
	 *             It is updated on each call to this function.
	 *             DO NOT CHANGE THIS VALUE BETWEEN CALLS!
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    index:   The index of the next entry. [i]
	 *    name:    Name of current vars entry. [c]
	 *    value:   Encoded value of current vars entry. [c]
	 *             Several special values are possible:
	 *             = "LIST" if entry is a VARS sublist.
	 *             = "DATA BLOCK" if entry is an indirect data block.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    encodevvalue:  Set to .TRUE. if the current entry was valid.
	 *                   Set to .FALSE. if end of vars list found
	 *                   or an error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  vars/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    vars:    descindex, VALUENIL, VALUELIST, VALUEDATA ...
	 *    mem:     sacmem
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    vars:    descindex, desclength, namelength, valuelength
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  decodevdesc, zgetc, cnvfta, cnvita, copydouble
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890321:  Added output of double precision values.
	 *    881111:  Expanded data types and added output of multiple values.
	 *    870916:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881111
	 *===================================================================== */
	/* PROCEDURE: */
	ncvmax = (value_s - 1);

	/* - Using the current vars entry pointer, decode the vars descripter,
	 *   saving descriptor information in vars common block variables. */

	cmvars.descindex = *index;
        offset = cmvars.descindex - cmvars.varsnode1;
	decodevdesc( cmmem.sacmem[cmvars.varsnode1]+offset, &cmvars.deleteflag, &cmvars.readonlyflag, 
	 &cmvars.indirectflag, &cmvars.sharedflag, &cmvars.reservedflag, 
	 &cmvars.applflag1, &cmvars.applflag2, &cmvars.valuetype, &cmvars.desclength, 
	 &cmvars.namelength, &cmvars.valuelength );

	/* - If type is a NIL, signifying the end of the vars list, set function
	 *   value to FALSE, and describe condition in output arguments. */

	if( cmvars.valuetype == VALUENIL ){
		encodeventry_v = FALSE;
		fstrncpy( name, name_s-1, "WARNING", 7 );
		fstrncpy( value, value_s-1, "END OF VARS LIST", 16 );

		/* - Otherwise, set function value to TRUE, return vars entry name,
		 *   encode as many vars values as will fit in the input string, 
		 *   and increment the vars entry pointer. */

		}
	else{
		encodeventry_v = TRUE;
		*index = cmvars.descindex + cmvars.desclength;
		fstrncpy( name, name_s-1, " ", 1 );
                offset = *index - cmvars.varsnode1;
		zgetc( cmmem.sacmem[cmvars.varsnode1]+offset, name, 4*cmvars.namelength );
		*index = *index + cmvars.namelength;
		fstrncpy( value, value_s-1, " ", 1 );
		ncv1 = 1;
		jvalue = 1;
		done = FALSE;
		if( cmvars.valuetype == VALUELIST ){
			fstrncpy( value, value_s-1, "LIST", 4 );
			}
		else if( cmvars.valuetype == VALUEDATA ){
			fstrncpy( value, value_s-1, "DATA BLOCK", 10 );
			}
		else if( cmvars.valuetype == VALUESTRING ){
			fstrncpy( value, value_s-1, " ", 1 );
                        offset = *index - cmvars.varsnode1;
			zgetc( cmmem.sacmem[cmvars.varsnode1]+offset, value, 4*cmvars.valuelength );
			}
		else if( cmvars.valuetype == VALUEINTEGER ){
L_1000:
			if( !done ){
                                offset = *index - cmvars.varsnode1;
				cnvita( (uintptr_t)cmmem.sacmem[cmvars.varsnode1]+offset, temp,17 );
				*index = *index + 1;
				ljust( temp,17 );
				nc = indexb( temp,17 );
				ncv2 = ncv1 + nc;
				if( (ncv2 + 4) <= ncvmax ){
                                        strncpy((s1=malloc(nc+1)),temp,nc);
                                        s1[nc] = '\0';
					subscpy( value, ncv1 - 1, ncv2 - 1, value_s - 
					 1, s1 );
                                        free(s1);
					ncv1 = ncv2 + 2;
					}
				else{
					subscpy( value, ncv1 - 1, -1, value_s - 1, "..."
					  );
					done = TRUE;
					}
				jvalue = jvalue + 1;
				if( jvalue > cmvars.valuelength )
					done = TRUE;
				goto L_1000;
				}
			}
		else if( cmvars.valuetype == VALUEUNSINT ){
			fstrncpy( value, value_s - 1, "UNSIGNED INTEGER", 16 );
			}
		else if( cmvars.valuetype == VALUESHORTINT ){
			fstrncpy( value, value_s - 1, "SHORT INTEGER", 13 );
			}
		else if( cmvars.valuetype == VALUEUNSSHORTINT ){
			fstrncpy( value, value_s - 1, "UNSIGNED SHORT INTEGER", 22);
			}
		else if( cmvars.valuetype == VALUEBYTE ){
			fstrncpy( value, value_s - 1, "BYTE", 4 );
			}
		else if( cmvars.valuetype == VALUEUNSBYTE ){
			fstrncpy( value, value_s - 1, "UNSIGNED BYTE", 13 );
			}
		else if( cmvars.valuetype == VALUEREAL ){
L_1600:
			if( !done ){
                                offset = *index - cmvars.varsnode1;
				cnvfta( cmmem.sacmem[cmvars.varsnode1]+offset, 16, 5, temp,17 );
				*index = *index + 1;
				ljust( temp,17 );
				nc = indexb( temp,17 );
				ncv2 = ncv1 + nc;
				if( (ncv2 + 4) <= ncvmax ){
                                        strncpy((s1=malloc(nc+1)),temp,nc);
                                        s1[nc] = '\0';
					subscpy( value, ncv1 - 1, ncv2 - 1, value_s - 
					 1, s1 );
                                        free(s1);
					ncv1 = ncv2 + 2;
					}
				else{
					subscpy( value, ncv1 - 1, -1, value_s - 1, "..."
					  );
					done = TRUE;
					}
				jvalue = jvalue + 1;
				if( jvalue > cmvars.valuelength )
					done = TRUE;
				goto L_1600;
				}
			}
		else if( cmvars.valuetype == VALUEDOUBLE ){
L_1700:
			if( !done ){
                                offset = *index - cmvars.varsnode1;
                                Sacmem = cmmem.sacmem[cmvars.varsnode1]+offset;
				copydouble( (double*)Sacmem, 1, (double*)&doubletemp );
                                sprintf(dtemp,"%24.5f",doubletemp);
				*index = *index + 2;
				ljust( dtemp,25 );
				nc = indexb( dtemp,25 );
				ncv2 = ncv1 + nc;
				if( (ncv2 + 4) <= ncvmax ){
                                        strncpy((s1=malloc(nc+1)),dtemp,nc);
                                        s1[nc] = '\0';
					subscpy( value, ncv1 - 1, ncv2 - 1, value_s - 
					 1, s1 );
                                        free(s1);
					ncv1 = ncv2 + 2;
					}
				else{
					subscpy( value, ncv1 - 1, -1, value_s - 1, "..."
					  );
					done = TRUE;
					}
				jvalue = jvalue + 2;
				if( jvalue > cmvars.valuelength )
					done = TRUE;
				goto L_1700;
				}
			}
		else if( cmvars.valuetype == VALUECOMPLEX ){
			fstrncpy( value, value_s - 1, "COMPLEX REAL", 12 );
			}
		else if( cmvars.valuetype == VALUEDBLCOMPLEX ){
			fstrncpy( value, value_s - 1, "DOUBLE PRECISION COMPLEX REAL", 29);
			}
		else if( cmvars.valuetype == VALUELOGICAL ){
L_2000:
			if( !done ){
                                offset = *index - cmvars.varsnode1;
				if( *(cmmem.sacmem[cmvars.varsnode1]+offset) != 0 ){
					strcpy( temp, "TRUE            " );
					nc = 4;
					}
				else{
					strcpy( temp, "FALSE           " );
					nc = 5;
					}
				*index = *index + 1;
				ncv2 = ncv1 + nc;
				if( (ncv2 + 4) <= ncvmax ){
                                        strncpy((s1=malloc(nc+1)),temp,nc);
                                        s1[nc] = '\0';
					subscpy( value, ncv1 - 1, ncv2 - 1, value_s - 
					 1, s1 );
                                        free(s1);
					ncv1 = ncv2 + 2;
					}
				else{
					subscpy( value, ncv1 - 1, -1, value_s - 1, "..."
					  );
					done = TRUE;
					}
				jvalue = jvalue + 1;
				if( jvalue > cmvars.valuelength )
					done = TRUE;
				goto L_2000;
				}
			}
		else if( cmvars.valuetype == VALUESYMBOL ){
			fstrncpy( value, value_s - 1, " ", 1 );
                        offset = *index - cmvars.varsnode1;
			zgetc( cmmem.sacmem[cmvars.varsnode1]+offset, value, 4*cmvars.valuelength );
			}
		else{
			fstrncpy( value, value_s - 1, "UNKNOWN VARS TYPE", 17 );
			encodeventry_v = FALSE;
			}
		cmvars.descindex = cmvars.descindex + cmvars.desclength + 
		 cmvars.namelength + cmvars.valuelength;
		*index = cmvars.descindex;
		}

L_8888:
	return( encodeventry_v );

} /* end of function */

