#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ settextfont(ifont)
int ifont;
{
	char kfile[MCPFN+1], kiline[MCMSG+1], fontbuf[11];
	int i, igtfn, j, jfont, ncerr, nerr;
        FILE *nun;
	void zbasename();

	for( i = 0 ; i < MCPFN ; i++ )
	    kfile[ i ] = ' ' ;
	kfile[ MCPFN ] = '\0' ;

	/*=====================================================================
	 * PURPOSE:  To set the software quality graphics text font.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ifont:   The number of the new font. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    mcpfn, ksubdl, kdirdl
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     kgtfn, iofset, ispace, iyoff, iyhigh, maxstr,
	 *             ascstr, stxmin, stxmax, stroke, nfont
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  zgtfun, crname, zopen, zclose
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870923:  Deleted ".saf" from aux file names.
	 *    850205:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  851209
	 *===================================================================== */
	/* PROCEDURE: */
	/* - This version assumes the existence of 4 files
	 *   containing the even numbered (block) Hershey fonts.
	 *   The odd numbered fonts are italicized versions of these same files. */
	/*    Font 1 -- Simplex Block.
	 *    Font 2 -- Simplex Italics.
	 *    Font 3 -- Duplex Block.
	 *    Font 4 -- Duplex Italics.
	 *    Font 5 -- Complex Block.
	 *    Font 6 -- Complex Italics.
	 *    Font 7 -- Triplex Block.
	 *    Font 8 -- Triplex Italics. */
	/* - These 8 fonts are repeated if the requested font is greater than 8. */
	/* - Ignore if the font is already loaded. */
	jfont = ((ifont - 1)%8) + 1;
	if( jfont == cmgdm.nfont || ((jfont - 1) == cmgdm.nfont && (jfont%
	 2) == 0) )
		goto L_8888;

	/* - If new font, create the file name,  open the file, and
	 *   read the data into the font tables. */

	zbasename( kfile,MCPFN+1 );
	crname( kfile,MCPFN+1, KSUBDL, "fonts",6, &nerr );
	if( nerr != 0 )
		goto L_9000;
	igtfn = (ifont + 1)/2;
	crname( kfile,MCPFN+1, KDIRDL, (char*)kmgdm.kgtfn[igtfn - 1]
	 ,41, &nerr );
	if( nerr != 0 )
		goto L_9000;
	zopens( &nun, kfile,MCPFN+1, "ROTEXT",7, &nerr );
	if( nerr != 0 )
		goto L_9000;

	/* - Read in base parameters. */

        if(fgets(kiline,MCMSG,nun)==NULL)goto L_9100;
        if(sscanf(kiline,"%3d%3d%3d%3d%5d",  &cmgdm.iofset, 
	 &cmgdm.ispace, &cmgdm.iyoff, &cmgdm.iyhigh, &cmgdm.maxstr) != 5){
          printf("error reading base parameters-settextfont\n");
          goto L_9100;
	}

	/* - Read in the ASCII lookup table and x-bounds. */

        fontbuf[10] = '\0';

	for( i = 1; i <= 121; i += 8 ){
                if(fgets(kiline,MCMSG,nun)==NULL)goto L_9100;
		for( j = i; j <= (i + 7); j++ ){
                   strncpy(fontbuf,kiline+((j-i)*10),10);
                   if(sscanf(fontbuf,"%4hd%3hd%3hd", &Ascstr[j], &Stxmin[j], 
			 &Stxmax[j] ) != 3){
                      printf("error reading ASCII lookup table-settextfont\n");
                      goto L_9100;
	           }
		}
	}

	/* - Read in the stroke table for the font. */

	for( i = 1; i <= (cmgdm.maxstr - 11); i += 12 ){
                if(fgets(kiline,MCMSG,nun)==NULL)goto L_9100;
		for( j = i; j <= (i + 11); j++ ){
                   if(sscanf(kiline+6+((j-i)*6),"%6hd",&Stroke[j] ) != 1){
                      printf("error reading stroke table-settextfont\n");
                      goto L_9100;
	           }
		}
	}

	/* - Close the font  file. */

	zcloses( &nun, &ncerr );

	/* - Save the font number and return. */

L_8888:
	cmgdm.nfont = ifont;
	return;

	/* - Process any errors here. */

L_9000:
	nerr = 2204;
	setmsg( "ERROR", nerr );
	apimsg( ifont );
	apcmsg( (char*)kmgdm.kgtfn[igtfn - 1],41 );
	goto L_8888;

L_9100:
	nerr = 2205;
	setmsg( "ERROR", nerr );
	apimsg( ifont );
	apcmsg( (char*)kmgdm.kgtfn[igtfn - 1],41 );
	goto L_8888;

} /* end of function */

