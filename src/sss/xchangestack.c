#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/sss.h"
void /*FUNCTION*/ xchangestack(nerr)
int *nerr;
{
	char kfile[MCPFN+1];
	int ldlyi, ldlyt;
	int ic1, ic2, jdfl, ncfile;
	float delay, delayi;



	/*=====================================================================
	 * PURPOSE: To parse the parameter-setting command CHANGESTACK.
	 *          This command changes properties of a file in stack.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001, 5106, 5107.
	 *=====================================================================
	 * MODULE/LEVEL:  sss/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    mcpfn
	 *    dfm:     ndfl, kdfl
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    sss:     dlyn, dlyt, dlyni, dlyti, wt, dst, lpol, del, beginTime,
	 *             endTime
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, cfmt, cresp, lcint, lcdfl, lkreal, lclog2,
	 *             setmsg, apimsg, apcmsg
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    kfile:   Name of file whose properties are to be changed. [c]
	 *    ncfile:  Number of characters in kfile. [i] {NOT USED}
	 *    jdfl:    Index of file whose properties are to be changed. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    960701:  Added beginTime and endTime, maf
	 *    850812:  Major rewrite of subprocess.
	 *    821130:  Changed to new command parsing logic.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850812
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Parse position dependent tokens.
	 *   (This is the name or number of the file whose properties are to be changed.) */

	if( lcint( &jdfl ) ){
		if( jdfl < 1 || jdfl > cmdfm.ndfl ){
			*nerr = 5107;
			setmsg( "ERROR", *nerr );
			apimsg( jdfl );
			goto L_8888;
			}
		}
	else if( lcchar( MCPFN, kfile,MCPFN+1, &ncfile ) ){
		jdfl = nfndcl( kmdfm.kdfl,MAXCHARS, kfile,MCPFN+1, &ic1, &ic2 );
		if( jdfl <= 0 ){
			*nerr = 5106;
			setmsg( "ERROR", *nerr );
			apcmsg( kfile,MCPFN+1 );
			goto L_8888;
			}
		}
	else{
		cfmt( "NEED A FILENAME OR NUMBER$",27 );
		cresp();
		}

	/* - Loop on rest of tokens in command:
	 *   (These are keywords which change properties for this file only.) */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "WEIGHT v":  define global weight property. */
		if( lkreal( "WEIGHT$",8, &Wt[jdfl] ) ){
			}

			/* -- "DELAY v":  define global static delay propertys. */
		else if( lkreal( "DE#LAY$",8, &delay ) ){
			if( lckey( "SECONDS$",9 ) ){
				ldlyt = TRUE;
				}
			else if( lckey( "POINTS$",8 ) ){
				ldlyt = FALSE;
				}
			if( ldlyt ){
				Dlyt[jdfl] = delay;
				}
			else{
				Dlyn[jdfl] = delay;
				}
			}

			/* -- "INCREMENT v":  define global static delay propertys. */
		else if( lkreal( "INCREMENT$",11, &delayi ) ){
			if( lckey( "SECONDS$",9 ) ){
				ldlyi = TRUE;
				}
			else if( lckey( "POINTS$",8 ) ){
				ldlyi = FALSE;
				}
			if( ldlyi ){
				Dlyti[jdfl] = delay;
				}
			else{
				Dlyni[jdfl] = delay;
				}
			}

			/* -- "NORMAL/REVERSED":  define global polarity property. */
		else if( lclog2( "NORMAL$",8, "REVERSED$",10, &Lpol[jdfl] ) ){
			}

			/* -- "DISTANCE v":  define global distance property. */
		else if( lkreal( "DI#STANCE$",11, &Dst[jdfl] ) ){
			}

                        /* -- "BEGINTIME v":  define global begin time property. added 960701 maf */
                else if( lkreal( "BE#GINTIME$",12, &Tbegin[jdfl] ) ){
                        }

                        /* -- "ENDTIME v":  define global end time property. added 960701 maf */
                else if( lkreal( "END#TIME$",10, &Tend[jdfl] ) ){
                        }

			/* -- Bad syntax. */
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;

		}

	/* - The above loop is over when one of two conditions has been met.:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0.
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

} /* end of function */

