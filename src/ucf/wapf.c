#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/eam.h"
void /*FUNCTION*/ wapf()
{
	char kapfln[133];
	int ic1, ic2, j, nexday, npkmsc, npksec;
        char *s1;


	/*=====================================================================
	 * PURPOSE: To write a pick to alphanumeric pick file (APF).
	 *=====================================================================
	 * MODULE/LEVEL:  EAM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    HDR:     NZYEAR, NZJDAY, NZHOUR, NZMIN, NZSEC, NZMSEC
	 *    DFM:     KDFL, IDFLC
	 *    EAM:     All.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INCTIM, INCDAT
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    860319:  Added PTP option.
	 *    810105:  Changed Ew.d formats to Gw.d.
	 *    800829:  Added ability to write times as GMT or not.
	 *             Added ability to write either header info or filename.
	 *    800505:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870923
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Each pick consists of a pick_id, event_id, station_id, component_id,
	 *   and pick source (automatic, manual etc.), as well as the date, time,
	 *   and amplitude of the pick itself. */
	/* - For certain picks, auxiliary information is also added to the card. */
	/* - Get filename. */
	lnumcl( kmdfm.kdfl,MAXCHARS, cmdfm.idflc, &ic1, &ic2 );

	/* - Each pick output line can contain 200 columns. */

	if( cmeam.lpfgmt ){
		inctim( *nzhour, *nzmin, *nzsec, *nzmsec, cmeam.pkseci, &cmeam.npkhr, 
		 &cmeam.npkmn, &npksec, &npkmsc, &nexday );
		cmeam.pksecs = tosecs( npksec, npkmsc );
		incdat( *nzyear, *nzjday, nexday, &cmeam.npkyr, &cmeam.npkjdy );
		}

	if( cmeam.lpfgmt && cmeam.lpfstd ){
		if( (strcmp(kmeam.kpkid,"MWF     ") == 0 || strcmp(kmeam.kpkid
		 ,"WF      ") == 0) || strcmp(kmeam.kpkid,"WAWF    ") == 0
		  ){
                        if (sprintf(kapfln,"%16s%8s%7.2f%7.2f%4s%5d%3d%3d%3d%6.2f %10.4g %1s %3s%c"
                         ,kevnm, kstnm, *cmpaz, *cmpinc, kmeam.kpkid, cmeam.npkyr,
			 cmeam.npkjdy, cmeam.npkhr, cmeam.npkmn, cmeam.pksecs, 
			 cmeam.pkampl, kmeam.kpksrc, kmeam.kpkrid, 'B' ) < 0) goto L_8000;

			for( j = 2; j <= 5; j++ ){
                                if(sprintf(kapfln+80+((j-2)*6),"%6.3f",Dtwf[j]) < 0)
                                                                    goto L_8000;
				}
                        if( sprintf(kapfln+80+(4*6)," %10.4g %10.4g\n", Awf[2], Awf[4] )
                                                  < 0 ) goto L_8000;
			}
		else if( strcmp(kmeam.kpkid,"PTP     ") == 0 ){
                        if(sprintf(kapfln,"%16s%8s%7.2f%7.2f%4s%5d%3d%3d%3d%6.2f %10.4g %1s %3s%c%6.3f%10.4g\n",
			 kevnm, kstnm, *cmpaz, *cmpinc, kmeam.kpkid, cmeam.npkyr, 
			 cmeam.npkjdy, cmeam.npkhr, cmeam.npkmn, cmeam.pksecs, 
			 cmeam.pkampl, kmeam.kpksrc, kmeam.kpkrid, 'B', Dtwf[4], 
			 Awf[4] ) < 0) goto L_8000;

			}
		else{
                        if(sprintf(kapfln,"%16s%8s%7.2f%7.2f%4s%5d%3d%3d%3d%6.2f %10.4g %1s %3s %c\n",
			 kevnm, kstnm, *cmpaz, *cmpinc, kmeam.kpkid, cmeam.npkyr, 
			 cmeam.npkjdy, cmeam.npkhr, cmeam.npkmn, cmeam.pksecs, 
			 cmeam.pkampl, kmeam.kpksrc, kmeam.kpkrid, 'B' ) < 0) goto L_8000;

			}
		}
	else if( cmeam.lpfgmt && !cmeam.lpfstd ){
		if( (strcmp(kmeam.kpkid,"MWF     ") == 0 || strcmp(kmeam.kpkid
		 ,"WF      ") == 0) || strcmp(kmeam.kpkid,"WAWF    ") == 0
		  ){
                        strncpy((s1=malloc(ic2-ic1+2)),kmdfm.kdfl+ic1-1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
                        if(sprintf(kapfln,"%32s      %4s%5d%3d%3d%3d%6.2f %10.4g %1s %3s%c",
			   s1, kmeam.kpkid, cmeam.npkyr, cmeam.npkjdy, cmeam.npkhr, cmeam.npkmn, 
			   cmeam.pksecs, cmeam.pkampl, kmeam.kpksrc, kmeam.kpkrid
			   , 'C' ) < 0) {
                             free(s1);
                             goto L_8000;
		        }
                        free(s1);
			for( j = 2; j <= 5; j++ ){
                                if(sprintf(kapfln+80+((j-2)*6),"%6.3f",Dtwf[j] ) < 0)
                                                       goto L_8000;
				}
                        if(sprintf(kapfln+80+(4*6)," %10.4g %10.4g\n", Awf[2], Awf[4] )
                                                      < 0) goto L_8000;
			}
		else if( strcmp(kmeam.kpkid,"PTP     ") == 0 ){
                        strncpy((s1=malloc(ic2-ic1+2)),kmdfm.kdfl+ic1-1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
                        if(sprintf(kapfln,"%32s      %4s%5d%3d%3d%3d%6.2f %10.4g %1s %3s%c%6.3f%10.4g\n",
			 s1, kmeam.kpkid, cmeam.npkyr, cmeam.npkjdy, cmeam.npkhr, cmeam.npkmn, 
			 cmeam.pksecs, cmeam.pkampl, kmeam.kpksrc, kmeam.kpkrid
			 , 'C', Dtwf[4], Awf[4] ) < 0) {
                          free(s1);
                          goto L_8000;
		        }
                        free(s1);
			}
		else{
                        strncpy((s1=malloc(ic2-ic1+2)),kmdfm.kdfl+ic1-1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
                        if(sprintf(kapfln,"%32s      %4s%5d%3d%3d%3d%6.2f %10.4g %1s %3s%c\n",
			 s1, kmeam.kpkid, cmeam.npkyr, cmeam.npkjdy, cmeam.npkhr, cmeam.npkmn, 
			 cmeam.pksecs, cmeam.pkampl, kmeam.kpksrc, kmeam.kpkrid
			 , 'C' ) < 0) {
                          free(s1);
                          goto L_8000;
		        }
                        free(s1);
		      }
	      }
	else if( !cmeam.lpfgmt && cmeam.lpfstd ){
		if( (strcmp(kmeam.kpkid,"MWF     ") == 0 || strcmp(kmeam.kpkid
		 ,"WF      ") == 0) || strcmp(kmeam.kpkid,"WAWF    ") == 0
		  ){
                        if(sprintf(kapfln,"%16s%8s%7.2f%7.2f%4s          %10.4g %10.4g %1s %3s%c",
			 kevnm, kstnm, *cmpaz, *cmpinc, kmeam.kpkid, cmeam.pkseci, 
			 cmeam.pkampl, kmeam.kpksrc, kmeam.kpkrid, 'D' ) < 0) goto L_8000;

			for( j = 2; j <= 5; j++ ){
                                if(sprintf(kapfln+80+((j-2)*6),"%6.3f", Dtwf[j] )
                                                         < 0) goto L_8000;
				}
                        if(sprintf(kapfln+80+(4*6)," %10.4g %10.4g\n", Awf[2], Awf[4] )
                                                         < 0) goto L_8000;
			}
		else if( strcmp(kmeam.kpkid,"PTP     ") == 0 ){
                        if(sprintf(kapfln,"%16s%8s%7.2f%7.2f%4s          %10.4g %10.4g %1s %3s%c%6.3f%10.4g\n",
			 kevnm, kstnm, *cmpaz, *cmpinc, kmeam.kpkid, cmeam.pkseci, 
			 cmeam.pkampl, kmeam.kpksrc, kmeam.kpkrid, 'D', Dtwf[4], 
			 Awf[4] ) < 0) goto L_8000;
			}
		else{
                        if(sprintf(kapfln,"%16s%8s%7.2f%7.2f%4s          %10.4g %10.4g %1s %3s%c\n",
			 kevnm, kstnm, *cmpaz, *cmpinc, kmeam.kpkid, cmeam.pkseci, 
			 cmeam.pkampl, kmeam.kpksrc, kmeam.kpkrid, 'D' ) < 0) goto L_8000;
			}
		}
	else{
		if( (strcmp(kmeam.kpkid,"MWF     ") == 0 || strcmp(kmeam.kpkid
		 ,"WF      ") == 0) || strcmp(kmeam.kpkid,"WAWF    ") == 0
		  ){
                        strncpy((s1=malloc(ic2-ic1+2)),kmdfm.kdfl+ic1-1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
                        if(sprintf(kapfln,"%32s      %4s          %10.4g %10.4g %1s %3s%c",
                                      s1, kmeam.kpkid, cmeam.pkseci, 
			              cmeam.pkampl, kmeam.kpksrc, kmeam.kpkrid, 'E' ) < 0) {
                          free(s1);
                          goto L_8000;
		        }
                        free(s1);
			for( j = 2; j <= 5; j++ ){
                                if(sprintf(kapfln+80+((j-2)*6),"%6.3f", Dtwf[j]) < 0) goto L_8000;
				}
                        if(sprintf(kapfln+80+(4*6)," %10.4g %10.4g\n", Awf[2], Awf[4] ) < 0)
                                                                                      goto L_8000;
			}
		else if( strcmp(kmeam.kpkid,"PTP     ") == 0 ){
                        strncpy((s1=malloc(ic2-ic1+2)),kmdfm.kdfl+ic1-1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
                        if(sprintf(kapfln,"%32s      %4s          %10.4g %10.4g %1s %3s%c%6.3f%10.4g\n",
			 s1, kmeam.kpkid, cmeam.pkseci, cmeam.pkampl, kmeam.kpksrc, kmeam.kpkrid
			 , 'E', Dtwf[4], Awf[4] ) < 0) {
                          free(s1);
                          goto L_8000;
		        }
                        free(s1);
			}
		else{
                        strncpy((s1=malloc(ic2-ic1+2)),kmdfm.kdfl+ic1-1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
                        if(sprintf(kapfln,"%32s      %4s          %10.4g %10.4g %1s %3s%c\n",
                         s1, kmeam.kpkid, cmeam.pkseci, 
			 cmeam.pkampl, kmeam.kpksrc, kmeam.kpkrid, 'E' ) < 0) {
                          free(s1);
                          goto L_8000;
		        }
			free(s1);
			}
		}

	/* - Write encoded character string to APF. */

L_8000:
        fprintf(cmeam.napfun,"%s\n",kapfln);

L_8888:
	return;

} /* end of function */

