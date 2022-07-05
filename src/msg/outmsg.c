#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/msg.h"
#include "../../inc/gdm.h"
void outmsg(void)
{
	int jlimsg, jlimsg_, junit, junit_, nc;
        char *s1, *message;
        char bell = 0x07;

	/*=====================================================================
	 * PURPOSE:  To write output message to the appropriate devices.
	 *=====================================================================
	 * MODULE/LEVEL:  msg/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MUNOUT
	 *    msg:     itpmsg, nummsg, klimsg, nlimsg, nunits, iunits
	 *=====================================================================
	 * GLOBAL COUPLING:  See SETMSG
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890106:  Added logic for processed (evaluated) command lines.
	 *    881230:  Added output to multiple file units.
	 *    870811:  Added logic to compute end of each text line.
	 *    860213:  Added output of message number for error messages.
	 *    860130:  Moved prefix logic from SETMSG to here.
	 *             This allows the prefix to be changed before writing.
	 *    830916:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881230
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Add a prefix to first line of message if appropriate.
	 *   (There is an ASCII BEL embedded in the error prefix.) */
	nc = max( 1, indexb( (char*)kmmsg.klimsg[0],MCMSG+1 ) );
        s1 = (char *)malloc(nc+1);
        message = (char *)malloc(nc+100+1); 
        strncpy(s1,kmmsg.klimsg[0],nc); s1[nc] ='\0';
	if( cmmsg.itpmsg == MERRORS ){
		if( cmmsg.nummsg >= 1000 ){
			for( junit = 1; junit <= cmmsg.nunits; junit++ ){
				junit_ = junit - 1;
				if( cmmsg.lsend[junit_][cmmsg.itpmsg - 1] ){

                                        sprintf(message,"%s%4d%s%s",
					 " ERROR ", cmmsg.nummsg, ": ", s1);
					  fputc((int)bell,cmmsg.iunits[junit_]);
                                          fprintf(cmmsg.iunits[junit_],"%s\n", message);
					}
				}
			}
		else{
			for( junit = 1; junit <= cmmsg.nunits; junit++ ){
				junit_ = junit - 1;
				if( cmmsg.lsend[junit_][cmmsg.itpmsg - 1] ){

                                        sprintf(message,"%s %3d%s%s",
					 " ERROR ", cmmsg.nummsg, ": ", s1);

					  fputc((int)bell,cmmsg.iunits[junit_]);
                                          fprintf(cmmsg.iunits[junit_],"%s\n",  message);
					}
				}
			}
		}
	else if( cmmsg.itpmsg == MWARNINGS ){
		for( junit = 1; junit <= cmmsg.nunits; junit++ ){
			junit_ = junit - 1;
			if( cmmsg.lsend[junit_][cmmsg.itpmsg - 1] ){

                                sprintf(message,"%s%s", " WARNING: ", s1 );
                                if( cmgdm.lgui ){
                                  ;
				}else
                                  fprintf(cmmsg.iunits[junit_],"%s\n", message );
				}
			}
		}
	else if( cmmsg.itpmsg == MPROCESSED ){
		for( junit = 1; junit <= cmmsg.nunits; junit++ ){
			junit_ = junit - 1;
			if( cmmsg.lsend[junit_][cmmsg.itpmsg - 1] ){

                                sprintf(message,"%s%s", " ==> ", s1 );
                                if( cmgdm.lgui ){
                                  ;
				}else
                                  fprintf(cmmsg.iunits[junit_],"%s\n", message );
				}
			}
		}
	else{
		for( junit = 1; junit <= cmmsg.nunits; junit++ ){
			junit_ = junit - 1;
			if( cmmsg.lsend[junit_][cmmsg.itpmsg - 1] ){
                                if( cmgdm.lgui ){
                                  ;
                                }else
                                  fprintf(cmmsg.iunits[junit_]," %s\n", s1);
				}
			}
		}

        free(s1);
        free(message);

	/* - Write remaining lines of current message. */

	for( jlimsg = 2; jlimsg <= cmmsg.nlimsg; jlimsg++ ){
		jlimsg_ = jlimsg - 1;
		nc = max( 1, indexb( (char*)kmmsg.klimsg[jlimsg_],MCMSG+1 ) );
                s1 = (char *)malloc(nc+1);
                strncpy(s1,kmmsg.klimsg[jlimsg_],nc);
                s1[nc] = '\0';
		for( junit = 1; junit <= cmmsg.nunits; junit++ ){
			junit_ = junit - 1;
			if( cmmsg.lsend[junit_][cmmsg.itpmsg - 1] ){
                                if( cmgdm.lgui ){
                                  ;
				}else
                                  fprintf(cmmsg.iunits[junit_]," %s\n", s1);
				}
			}
                free(s1);
	}

L_8888:
	return;

} /* end of function */




