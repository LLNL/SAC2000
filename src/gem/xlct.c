#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>

#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/gdm.h"

void loadctable(char* name, char* directory, int* nentry, int* nerr);

void /*FUNCTION*/ xlct(nerr)
int *nerr;
{
	char _c0[2];

        char krddir[MCPFN+1];

        int nchar;
        int lnum, inum, nfiles; 
        char ktok[21], infile[21];
        char *cptr;

        static char *ctables[17] = {
            "bw.linear", "16.level", "16.level.II", "blu.grn.red.yellow",
            "blue.red", "blue.white", "green.pink", "grn.red.blu.wht",
            "grn.white.linear", "grn.wht.exponential", "prism", "red.purple",
            "red.temperature", "std.gamma.II", "steps", "wave.special",
            "color.tbl1.sac"
	};


	/*=====================================================================
	 * PURPOSE:  To execute the action command LOADCTABLE.
	 *           This command reads pseudocolor color tables
	 *           into SAC's memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *  
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *  
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 */

         strcpy(infile,"color.tbl1.sac");
         krddir[0] = '\0';

        while(lcmore(nerr)){
            if( lkchar( "DIR#$",6, MCPFN, krddir,MCPFN+1, &nchar ) ){
		if( memcmp(krddir,"CURRENT",7) == 0 ||
                    memcmp(krddir ,"current",7) == 0 )
		{
		    fstrncpy( krddir, MCPFN, " ", 1);
		}
		else if( krddir[nchar - 1] != KDIRDL ){
                    _c0[0] = KDIRDL;
                    _c0[1] = '\0';
		    subscpy( krddir, nchar, -1, MCPFN, _c0 );
		}

	    }
            /* find either a file name or number */ 
            else{
                if( lcdfl(ktok,21,&nfiles)){
                    strncpy(infile,ktok+1,20);
                    infile[20] = '\0';
                    cptr = infile;
                    while((*cptr != ' ') && (*cptr != '\0'))cptr++;
                    *cptr = '\0';

                    inum = strtol(infile,&cptr,0);
                    if( *cptr == '\0')lnum = TRUE;
                    else{
                        lnum = FALSE;
		    }

                    if(lnum){
                        /* the filename argument is an index into the ctables
                           array set a flag to pass to loadctable to tell it
                           to look in SACAUX */
                        if((inum <= 0) || (inum > 17) ){
                            printf("color table number must be between 1 and 17\n");
                            *nerr = 1;
                            goto L_8888;                      
		        }
                        strcpy(infile,ctables[inum-1]);
		    }
                    else{
                        /* the filename argument is an ascii name */
                        /* look in the current directory */
                        /* (or the directory specified in the DIR option) */
		    }
		}
	    }
        }

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
	    goto L_8888;

	/* EXECUTION PHASE: */

        /* make sure that infile is a null terminated string */
        loadctable(infile, krddir, &cmgdm.npscimage, nerr);

L_8888:

	return;

} /* end of function */

