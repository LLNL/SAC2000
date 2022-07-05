#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "complex.h"
#include "proto.h"
#define	MCL	1500

#include "mach.h"
#include "cnd.h"
#include "cpf.h"

void /*FUNCTION*/ xdo(nerr)
int *nerr;
{
	char kcl[MCL+1], kclin[MCL+1], kdirin[MCPFN+1], kmacroname[MCPFN+1], ktoken[9];
	int lexpand, lnumbr;
	int idx, inc, index1, index2, index3, 
	 nc, ncl, nclin, nverr, do_count;
	float fnumb;
        char *cattemp;
        char *strtemp;


	/*=====================================================================
	 * PURPOSE: To parse the action command DO.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: .
	 *=====================================================================
	 * MODULE/LEVEL:  cnd/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *     mach:   MCPW, kdirdl
	 *     cpf:    kvarsname
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *     cnd:    ndolevel: Incremented by one.
	 *             lifresp:  Set to value of if test.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  setmsg, apcmsg, getdolen, ictok, modcase, cnvita,
	 *             putcl, wildfl, putvvstring, skipdo
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    880915:  Fixed bug in getting filelists.
	 *    871014:  Enlarged size of character lists, added WILD option,
	 *             and added much more error checking.
	 *    870817:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  871014
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Initialize and do some error checking. */
	*nerr = 0;
	cnd.ndolevel = cnd.ndolevel + 1;

        memset(kcnd.kdovar[cnd.ndolevel - 1],(int)' ',MCPFN);
        kcnd.kdovar[cnd.ndolevel - 1][MCPFN] = '\0';
        memcpy(kcnd.kdovar[cnd.ndolevel - 1],kmcpf.kvarsname,strlen(kmcpf.kvarsname));

	Ndotype[cnd.ndolevel] = 0;

	/* - Get length of do loop */

	getdolen( &Ndolines[cnd.ndolevel], nerr );
	if( *nerr != 0 ){
	    apcmsg( "in macro file",14 );
	    getvvstring( kmcpf.kvarsname,9, "macroname",10, 
	      &nc, kmacroname ,MCPFN+1, &nverr );
	    apcmsg( kmacroname,MCPFN+1 );
	    outmsg () ;
	    clrmsg () ;
	    *nerr = 0 ;
	    goto L_8888;
	}

	/* - Get loop variable name */

	if( !lctok( (char*)kcnd.kdoname[cnd.ndolevel - 1],MCPFN+1, &lnumbr, 
	 &fnumb ) ){
	    *nerr = 2701;
	    setmsg( "ERROR", *nerr );
	    apcmsg( "after DO",9 );
	    goto L_8888;
	}
	ictok( 1 );

        memset(kcl,(int)' ',MCL);
        kcl[MCL] = '\0';
        memcpy(kcl,kcnd.kdoname[cnd.ndolevel - 1],strlen(kcnd.kdoname[cnd.ndolevel - 1]));

	idx = indexb( kcl,MCL+1 );
	subscpy( kcl, idx, idx + 3, MCL, "list" );
	strscpy( kcnd.kdolist[cnd.ndolevel - 1], kcl, MCPFN );

	if( !lctok( ktoken,9, &lnumbr, &fnumb ) ){
	    *nerr = 2701;
	    setmsg( "ERROR", *nerr );
            cattemp = malloc(6+strlen(kcnd.kdoname[cnd.ndolevel - 1])+1);
            strcpy(cattemp,"after ");
            strcpy(cattemp+6,kcnd.kdoname[cnd.ndolevel - 1]);
	    apcmsg( cattemp, 6+strlen(kcnd.kdoname[cnd.ndolevel - 1])+1);
            free(cattemp);
	    goto L_8888;
	}
	modcase( TRUE, ktoken, MCPW, ktoken );
	ictok( 1 );

	/* - Process do loop
	 *   Syntax: DO var = start , stop [, inc]  
	 *           DO var FROM start TO stop [BY inc] */

	if( ((memcmp(ktoken,"FROM",4) == 0) || (memcmp(ktoken
	 ,"from",4) == 0)) || (ktoken[0] == '=') ){

	    inc = 1;

	    if( !lctok( ktoken,9, &lnumbr, &fnumb ) ){
		*nerr = 2701;
		setmsg( "ERROR", *nerr );
                cattemp = malloc(6+strlen(ktoken)+1);
                strcpy( cattemp, "after ");
                strcat( cattemp, ktoken);
		apcmsg( cattemp, 6+strlen(ktoken)+1 );
                free(cattemp);
		goto L_8888;
	    }
	    ictok( 1 );
	    index1 = fnumb;

	    if( !lctok( ktoken,9, &lnumbr, &fnumb ) ){
		*nerr = 2701;
		setmsg( "ERROR", *nerr );
                cattemp = malloc(6+strlen(ktoken)+1);
                strcpy( cattemp, "after ");
                strcat( cattemp, ktoken);
		apcmsg( cattemp, 6+strlen(ktoken)+1 );
                free(cattemp);
		goto L_8888;
	    }
	    ictok( 1 );
	    if( ((memcmp(ktoken,"TO",2) != 0) && (memcmp(ktoken
	     ,"to",2) != 0)) && (ktoken[0] != ',') ){
		*nerr = 2701;
		setmsg( "ERROR", *nerr );
                cattemp = malloc(3+strlen(ktoken)+1);
                strcpy( cattemp, "at ");
                strcat( cattemp, ktoken);
		apcmsg( cattemp, 3+strlen(ktoken)+1 );
                free(cattemp);
		goto L_8888;
	    }

	    if( !lctok( ktoken,9, &lnumbr, &fnumb ) ){
		*nerr = 2701;
		setmsg( "ERROR", *nerr );
                cattemp = malloc(6+strlen(ktoken)+1);
                strcpy( cattemp, "after ");
                strcat( cattemp, ktoken);
		apcmsg( cattemp, 6+strlen(ktoken)+1 );
                free(cattemp);
		goto L_8888;
	    }
	    ictok( 1 );
	    index2 = fnumb;

	    if( lctok( ktoken,9, &lnumbr, &fnumb ) ){
		ictok( 1 );
		if( ((memcmp(ktoken,"BY",2) != 0) && (memcmp(ktoken
		 ,"by",2) != 0)) && (ktoken[0] != ',') ){
		    *nerr = 2701;
		    setmsg( "ERROR", *nerr );
		    cattemp = malloc(3+strlen(ktoken)+1);
		    strcpy( cattemp, "at ");
		    strcat( cattemp, ktoken);
		    apcmsg( cattemp, 3+strlen(ktoken)+1 );
		    free(cattemp);
		    goto L_8888;
		}

		if( !lctok( ktoken,9, &lnumbr, &fnumb ) ){
		    *nerr = 2701;
		    setmsg( "ERROR", *nerr );
		    cattemp = malloc(6+strlen(ktoken)+1);
		    strcpy( cattemp, "after ");
		    strcat( cattemp, ktoken);
		    apcmsg( cattemp, 6+strlen(ktoken)+1 );
		    free(cattemp);
		    goto L_8888;
		}
		inc = fnumb;
	    }

	    memset( kcl, ' ' , MCL );

            do_count = 0;

            if(index1 < index2){
 		if( inc <= 0 ){ do_count = 0;}
 		else {
 		    /*  do_count = (index2 - index1 + 1)/ inc;
 		    if((do_count == 0) ||((do_count % inc) != 0)) do_count++;    
 		    */
 		    for(idx = index1; idx <= index2; idx += inc) do_count++;
		}
	    }else if(index1 > index2){
		if( inc >= 0 ){ do_count = 0;}
		else {
		    /* do_count = (index1 - index2 + 1)/ abs(inc);
		    if((do_count == 0) || ((do_count % abs(inc)) != 0)) do_count++;
		    */
		    for(idx = index1; idx >= index2; idx += inc) do_count++;
		}
	    }else{ /* index1 == index2 */
                do_count = 1;
	    }

	    for( idx = index1; do_count > 0; idx += inc, do_count-- ){
		cnvita( idx, ktoken,9 );
		index3 = indexa( ktoken,9, ' ', TRUE, FALSE );

                strtemp = malloc(9-index3+1);
                strncpy(strtemp,ktoken+index3 - 1,9-index3);
                strtemp[9-index3] = '\0';

		putcl( kcl,MCL+1, strtemp,-index3 + 10, nerr );

                free(strtemp);

		if( *nerr != 0 ){
		    *nerr = 2702;
		    setmsg( "ERROR", *nerr );
		    apimsg( MCL );
		    goto L_8888;
		}
	    }
	}

	/* - Process Do List option.
	 *   Syntax:  DO var LIST value1 value2 value3 ... */
	else if( memcmp(ktoken,"LIST",4) == 0 || memcmp(ktoken ,"list",4) == 0 ){
	    if( lccl( kcl,MCL+1, &ncl ) )
	    { /* do nothing */ }
	}

	/* - Process Do Wild option.
	 *   Syntax:  DO var WILD [DIR dir] wild1 wild2 wild3 ... */
	else if( memcmp(ktoken,"WILD",4) == 0 || memcmp(ktoken ,"wild",4) == 0 ){
	    memset(kdirin,(int)' ',MCPFN);
	    kdirin[MCPFN] = '\0';

	    while ( lcmore( nerr ) ){
		if( lkchar( "DIR#$",6, MCPFN, kdirin,MCPFN+1, &nc ) ){
		    if( kdirin[nc - 1] != KDIRDL )
			kdirin[nc] = KDIRDL;
		}
		else if( lccl( kclin,MCL+1, &nclin ) )
		{ /* do nothing */ }
	    }

	    wildfl( kdirin,MCPFN+1, kclin,MCL+1, nclin, kcl,MCL+1, &ncl, &lexpand );
	}

	/* - Syntax error. */
	else{
	    *nerr = 2701;
	    setmsg( "ERROR", *nerr );
            cattemp = malloc(3+strlen(ktoken)+1);
            strcpy( cattemp, "at ");
            strcat( cattemp, ktoken);
	    apcmsg( cattemp, 3+strlen(ktoken)+1 );
            free(cattemp);
	    goto L_8888;

	}

	nc = indexb( kcl,MCL+1 ) + 2;
	putvvstring( (char*)kcnd.kdovar[cnd.ndolevel - 1],MCPFN+1,
	  (char*)kcnd.kdolist[cnd.ndolevel - 1],MCPFN+1, nc, kcl,MCL+1, nerr );

	if( *nerr != 0 )
		goto L_8888;
	Idoin1[cnd.ndolevel] = 0;
	Idoin2[cnd.ndolevel] = 0;
	if( !ldolist( nerr ) ){
	    skipdo( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

L_8888:
	if( *nerr != 0 )
	    cnd.ndolevel = cnd.ndolevel - 1;

	return;

} /* end of function */

