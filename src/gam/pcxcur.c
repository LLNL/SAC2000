#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gam.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ pcxcur(nunrpl)
FILE *nunrpl;
{
	char kmsg[MCMSG+1], ktext[MCMSG+1];
	int lend, lquit;
	static int lopt;
	byte kchar, kchar2;
	int i1, i2, icloc1, icloc2, icpntr, iop1, iop2, iope, iopei, 
	 itype, jope, nc, nctext, nerr, numchar;
        FILE *nunmac;
	float height, width, xcdpsv, xtloc, ycdpsv, ytloc;
        char *strtemp1, *strtemp2;


	/*=====================================================================
	 * PURPOSE:
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *=====================================================================
	 * MODULE/LEVEL:
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *    970129:  Add parameter (0) to cnvatf and cnvati.  0 means that if 
         *             a string of digits is too int, let it slide by.  maf 
	 *    830623:  Added enviroment options with arbitrary integer argument.
	 *    821013:  Added ability to delete previous command from replay file.
	 *    821008:  Original version.
	 *===================================================================== */
	/* PROCEDURE: */
L_5000:
	;

	/* - Set origin (ORI) to current data point (CDP) if requested. */

	if( !cmgam.lglori ){
		cmgam.xori = cmgam.xcdp;
		cmgam.yori = cmgam.ycdp;
		}

	/* - Get cursor location and character.  Convert character to upper case. */

	worldcursor( &cmgam.xcdp, &cmgam.ycdp, &kchar );
	upcase( &kchar, 1, &kchar, 1 );

	/* - Check for "termination op" command. */

	if( kchar == kmgam.kopqui )
		goto L_8888;

	/* - Check for "delete last op" command. */

	if( kchar == kmgam.kopdel ){
		if( lopt ){
			backspace( nunrpl,1L );

                        if(fgets(ktext,MCMSG+1,nunrpl) == NULL) goto L_8888;
                        if(ktext[(numchar=strlen(ktext)-1)] == '\n')ktext[numchar] = '\0';

			backspace( nunrpl,1L );
			if( memcmp(ktext,"        ",8) == 0 ){
L_3000:
				backspace( nunrpl,1L );

                                if(fgets(ktext,MCMSG+1,nunrpl) == NULL) goto L_8888;
                                if(ktext[(numchar=strlen(ktext)-1)] == '\n')ktext[numchar] = '\0';

				backspace( nunrpl,1L );
				if( ktext[0] != Kopt[2] )
					goto L_3000;
				}
			else{
				backspace( nunrpl,1L );
				pcrrpl( nunrpl, &kchar, &kchar2, &lend, &lquit );
				if( lend )
					goto L_5000;
				backspace( nunrpl,1L );
				}
			}
		else{
			backspace( nunrpl,1L );
			pcrrpl( nunrpl, &kchar, &kchar2, &lend, &lquit );
			/***BUG: Need logic to backspace over sector ops correctly*** */
			if( lend )
				goto L_5000;
			backspace( nunrpl,1L );
			}
		goto L_5000;
		}

	/* - Check for "comment line op". */

	if( kchar == kmgam.kopcmt ){
		cursortext( &xtloc, &ytloc, ktext,MCMSG+1 );
		nctext = indexb( ktext,MCMSG+1 );
                strncpy((strtemp1=malloc(nctext+1)),ktext,nctext);
                strtemp1[nctext] = '\0';

                fprintf(nunrpl,"%c%s\n",kchar,strtemp1);

                free(strtemp1);
		lopt = TRUE;
		goto L_5000;
		}

	/* - See if it is a "macro invocation request." */

	if( kchar == kmgam.kopmac ){
		xcdpsv = cmgam.xcdp;
		ycdpsv = cmgam.ycdp;
		cursortext( &xtloc, &ytloc, ktext,MCMSG+1 );
		nctext = indexb( ktext,MCMSG+1 );
		if( nctext > 0 ){
			icpntr = 0;
			poptok( ktext, nctext, &icpntr, &icloc1, &icloc2, &itype );

                        fstrncpy( kmgam.kpcmac, MCPFN, ktext+icloc1 - 
			 1,min(icloc2,MCMSG) - icloc1 + 1);
                        fstrncpy( kmgam.kpcmac+(min(icloc2,MCMSG) - icloc1 + 1),MCPFN -
                                  (min(icloc2,MCMSG) - icloc1 + 1),kmgam.kpcmsu,
                                  strlen(kmgam.kpcmsu));

			poptok( ktext, nctext, &icpntr, &icloc1, &icloc2, &itype );
			if( itype > 0 ){
                                strncpy((strtemp1=malloc(icloc2-icloc1+2)),
                                         ktext+icloc1-1,icloc2-icloc1+1);
                                strtemp1[icloc2-icloc1+1] = '\0';

				cnvatf( strtemp1, icloc2 - icloc1 + 2, &cmgam.scamac, 
				 0, &nerr );	/* add 0 before nerr. maf 970129 */

                                free(strtemp1);
				if( nerr != 0 )
					cmgam.scamac = 1.;
				}
			poptok( ktext, nctext, &icpntr, &icloc1, &icloc2, &itype );
			if( itype > 0 ){
                                strncpy((strtemp1=malloc(icloc2-icloc1+2)),
                                         ktext+icloc1-1,icloc2-icloc1+1);
                                strtemp1[icloc2-icloc1+1] = '\0';
        
				cnvatf( strtemp1, icloc2 - icloc1 + 2, &cmgam.rotmac, 
				 0, &nerr );	/* add 0 before nerr. maf 970129 */

                                free(strtemp1);
				if( nerr != 0 )
					cmgam.rotmac = 0.;
				}
			}

		zopens( &nunmac, kmgam.kpcmac,MCPFN+1, "TEXT",5, &nerr );
		if( nerr != 0 )
			goto L_8888;
		pcmrpl( nunmac, cmgam.scamac, cmgam.rotmac );

                fclose(nunmac);

		cmgam.xcdp = xcdpsv;
		cmgam.ycdp = ycdpsv;

                sprintf(kmsg,"%c%10.5f%10.5f", kmgam.kopmac, cmgam.xcdp, 
		 cmgam.ycdp );

                fprintf(nunrpl,"%s\n",kmsg);

                strncpy((strtemp1=malloc(nctext+1)),ktext,nctext);
                strtemp1[nctext] = '\0';

                fprintf(nunrpl,"%s\n",strtemp1);

                free(strtemp1);
		goto L_5000;
		}

	/* - See if it is a "change environment op". */

	if( kchar == kmgam.kopbe ){
		kmgam.kopetx[0] = kmgam.kopbe;
		jope = 2;
		worldcursor( &cmgam.xcdpe, &cmgam.ycdpe, &kmgam.kopetx[jope - 
		 1] );
L_5500:
		if( kmgam.kopetx[jope - 1] == kmgam.kopee ){
                        strncpy((strtemp1=malloc(jope+1)),kmgam.kopetx,jope);
                        strtemp1[jope] = '\0';

                        fprintf(nunrpl,"%s\n",strtemp1);

                        free(strtemp1);
			goto L_5000;
			}
		jope = jope + 1;
		worldcursor( &cmgam.xcdpe, &cmgam.ycdpe, &kmgam.kopetx[jope - 
		 1] );

                strncpy((strtemp1=malloc(3)),kmgam.kopetx+jope - 2,2);
                strtemp1[2] = '\0';
                strncpy((strtemp2=malloc(3)),kmgam.kopetx+jope - 2,2);
                strtemp2[2] = '\0';

		upcase( strtemp1, 2, strtemp2, jope - (jope - 1) + 2 );
		subscpy( kmgam.kopetx, jope - 2, jope - 1, 80, strtemp2 );

                free(strtemp1);

		iope = nccomp( strtemp2, (char*)kmgam.kope,3, cmgam.nope, 2 );
                free(strtemp2);

		if( iope < cmgam.nopei ){
			pcxope( iope, 0 );
			jope = jope + 1;
			worldcursor( &cmgam.xcdpe, &cmgam.ycdpe, &kmgam.kopetx[jope - 
			 1] );
			}
		else{
			iopei = 0;
			jope = jope + 1;
			worldcursor( &cmgam.xcdpe, &cmgam.ycdpe, &kmgam.kopetx[jope - 
			 1] );
			cnvati( &kmgam.kopetx[jope - 1],1, &i1, 0, &nerr );
								/* add 0. maf 970129 */
			if( nerr == 0 ){
				jope = jope + 1;
				worldcursor( &cmgam.xcdpe, &cmgam.ycdpe, &kmgam.kopetx[jope - 
				 1] );
				cnvati( &kmgam.kopetx[jope - 1],1, &i2, 0, &nerr );
								/* add 0. maf 970129 */
				if( nerr == 0 ){
					iopei = 10*i1 + i2;
					jope = jope + 1;
					worldcursor( &cmgam.xcdpe, &cmgam.ycdpe, &kmgam.kopetx[jope - 
					 1] );
					}
				else{
					iopei = i1;
					}
				}
			pcxope( iope, iopei );
			}
		goto L_5500;
		}

	/* - See if it is a "single op". */

	iop1 = nccomp( &kchar, kmgam.kop1, 1, cmgam.nop1, 1 );
	if( iop1 > 0 ){
                sprintf(kmsg,"%c%10.5f%10.5f", kchar, cmgam.xcdp, cmgam.ycdp );
                fprintf(nunrpl,"%s\n",kmsg);

		/* -- Set up data points if this is a "rectangle" opcode. */
		if( kchar == 'R' ){
			Xrect[1] = cmgam.xori;
			Yrect[1] = cmgam.ycdp;
			Xrect[2] = cmgam.xori;
			Yrect[2] = cmgam.yori;
			Xrect[3] = cmgam.xcdp;
			Yrect[3] = cmgam.yori;
			Xrect[4] = cmgam.xcdp;
			Yrect[4] = cmgam.ycdp;
			}
		pcxop1( iop1 );
		lopt = FALSE;
		goto L_5000;
		}

	/* - See if it is a "double op". */

	iop1 = nccomp( &kchar, kmgam.kop2, 1, cmgam.nop2, 1 );
	if( iop1 > 0 ){
L_6000:
		worldcursor( &cmgam.xcdp, &cmgam.ycdp, &kchar2 );
		upcase( &kchar2, 1, &kchar2, 1 );
		iop2 = 0;
		if( iop2 > 0 ){
                        fprintf(nunmac,"%s\n",kmsg);
			pcxop2( iop1, iop2 );
			}
		else{
                        fprintf(MUNOUT," %s%c\n", "Illegal character: ", kchar2 );
			goto L_6000;
			}
		lopt = FALSE;
		goto L_5000;
		}

	/* - See if it is a "n-point op". */

	iop1 = nccomp( &kchar, kmgam.kopn, 1, cmgam.nopn, 1 );
	if( iop1 == 1 ){
		Xopnli[1] = cmgam.xori;
		Xopnli[2] = cmgam.xcdp;
		Yopnli[1] = cmgam.yori;
		Yopnli[2] = cmgam.ycdp;

                sprintf(kmsg,"%c%10.5f%10.5f", kchar, cmgam.xcdp, cmgam.ycdp );

                fprintf(nunrpl,"%s\n",kmsg);

		worldcursor( &cmgam.xcdp, &cmgam.ycdp, &kchar2 );
		upcase( &kchar2, 1, &kchar2, 1 );

                sprintf(kmsg,"%c%10.5f%10.5f", kchar2, cmgam.xcdp, 
		 cmgam.ycdp );

                fprintf(nunrpl,"%s\n",kmsg);

		Xopnli[3] = cmgam.xcdp;
		Yopnli[3] = cmgam.ycdp;
		iop2 = 1;
		if( kchar2 == 'C' )
			iop2 = 2;
		pcxops( iop1, iop2, cmgam.xopnli, cmgam.yopnli, cmgam.pcdegi );
		lopt = FALSE;
		goto L_5000;
		}

	/* - See if it is a "text op". */

	iop1 = nccomp( &kchar, kmgam.kopt, 1, cmgam.nopt, 1 );
	if( iop1 > 0 ){
		/* -- Get text size and convert to world coordinates. */
		gettextsize( &width, &height );
		vporttoworld( width, height, &width, &height );
		lopt = TRUE;

                sprintf(kmsg,"%c%10.5f%10.5f", kchar, cmgam.xcdp, cmgam.ycdp );
                fprintf(nunrpl,"%s\n",kmsg);

L_7000:
		fstrncpy( ktext, MCMSG, " ", 1);
		cursortext( &xtloc, &ytloc, ktext,MCMSG+1 );
		nc = indexb( ktext,MCMSG+1 );

                strncpy((strtemp1=malloc(nc+1)),ktext,nc);
                strtemp1[nc] = '\0';

                fprintf(nunrpl,"%s\n",strtemp1);

                free(strtemp1);

		worldmove( cmgam.xcdp, cmgam.ycdp );
		if( nc > 0 ){
			text( ktext,MCMSG+1, nc );
			flushbuffer( &nerr );
			if( nerr != 0 )
				goto L_8888;
			cmgam.ycdp = cmgam.ycdp - height;
			if( iop1 == 2 )
				goto L_7000;
			}
		goto L_5000;
		}

	/* - If none of the above, then user has typed an illegal character. */

        fprintf(MUNOUT," %s%c\n", "Illegal character: ", kchar );
	goto L_5000;

L_8888:

	return;


} /* end of function */

