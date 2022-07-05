#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ ljust(ksym, ksym_s)
char *ksym;   int ksym_s;
{
	int i, count, ncout, ncsym, ncsave, nchar;
        char *temp;

	/*=====================================================================
	 * PURPOSE:  To left justify a character string.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KSYM:    Character string. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KSYM:    Left justified character string. [c]
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900322:  Fixed problem when ncout gt ncsym. (VAX/VMS bug fix.)
	 *    830811:  Original version (from ZLJUST).
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  871022
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine number of characters in symbol. */
	ncsym = (ksym_s - 1);
        temp = ksym;
        count = 0;
        while ( *temp != '\0' && (count <= ncsym) ){
          temp++;
          count++;
	}
        ncsym = count < ncsym ? count : ncsym;

        ncsave = ncsym;

	/* - Do loop excludes leading blanks. */
        temp = ksym;
        while( ( *temp == ' ' ) && (ncsym > 0) ){
          temp++;
          ncsym--;
	}

        if( (ncsym > 0) && (temp != ksym) ) {
          nchar = ncsave - (temp - ksym);
          for(i=0; i<nchar; i++){
            ksym[i] = temp[i];
	  }
          for(i=nchar; i< ncsave; i++) ksym[i] = ' ';
	}

L_8888:
	return;

} /* end of function */

