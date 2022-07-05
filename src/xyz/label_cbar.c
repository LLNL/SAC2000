#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
#include "gem.h"
#include "gam.h"
#include "gdm.h"


void cbar_window(unsigned int xloc,unsigned int yloc,unsigned int height,unsigned int w_height,unsigned int w_width,float vspaceratio,float ypmax,int* nerr);

void label_cbar(xloc, yloc, width, height, w_width, w_height,
		dmin, dmax, ypmax, nerr)
unsigned int xloc, yloc, width, height, w_width, w_height;
float dmin, dmax, ypmax;
int *nerr;
{

       int lrigaxsave, llefaxsave, lrigtcsave, lleftcsave,
	    lylabsave, ltitlsave;
       float ypmxsave, ypmnsave, xpmxsave, xpmnsave;
       int llydivsave, llnydivsave, nydivsave, lylimsave, lygrdsave;
       float yimnzsave, yimxzsave, ypmnusave, ypmxusave, xpmnusave;
       float vspaceratio;
  
	/*=====================================================================
	 * PURPOSE:
	 *        
         *        
         *        
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag.  Set to 0 if no error occurred.
	 *        
	 *=====================================================================
	 * MODULE/LEVEL:  gam/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    dfm:     ndfl, sacmem
	 *    hdr:     begin, ennd, delta
	 *    gem:     lbotax, lbottc, ltopax, ltoptc, lxlab, lylab, ltitl,
	 *             lxgrd, ypmn, ypmx, chht, tsdef
	 *    gam:     kgddef
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
         * 
	 *=====================================================================
	 * DOCUMENTED/REVIEWED: 
	 *===================================================================== */
	/* PROCEDURE: */

	*nerr = 0;

	lrigaxsave = cmgem.lrigax;
        llefaxsave = cmgem.llefax;
        lrigtcsave = cmgem.lrigtc;
        lleftcsave = cmgem.lleftc;
        llydivsave = cmgem.lydiv;
        llnydivsave = cmgem.lnydiv;
        lygrdsave   = cmgem.lygrd;
        nydivsave  = cmgem.nydiv;

        cmgem.lnydiv = TRUE;
        cmgem.nydiv = 4;
        cmgem.lydiv = FALSE;
        cmgem.lrigax = FALSE;
        cmgem.llefax = TRUE;
        cmgem.lrigtc = FALSE;
        cmgem.lleftc = TRUE;
        cmgem.lygrd = FALSE;

	lylabsave = cmgem.lylab;
	ltitlsave = cmgem.ltitl;

	cmgem.lylab = TRUE;
	cmgem.ltitl = FALSE;

	ypmxsave = cmgem.ypmx;
        ypmnsave = cmgem.ypmn;
        xpmxsave = cmgem.xpmx;
        xpmnsave = cmgem.xpmn;

        ypmnusave = cmgem.ypmnu;
        ypmxusave = cmgem.ypmxu;
        xpmnusave = cmgem.xpmnu;

        getratio(&vspaceratio);

/* calculate the window for ylinax to use in labeling the colorbar */

        cbar_window(xloc,yloc,height,w_height,w_width,vspaceratio, ypmax, nerr);
        
       	/* --- Set up y axis plot limits. */

        lylimsave = cmgem.lylim;
        yimnzsave = cmgem.yimnz;
        yimxzsave = cmgem.yimxz;

       	cmgem.lylim = TRUE;
       	cmgem.yimnz = dmin;
       	cmgem.yimxz = dmax;

     	cmgem.chht = cmgem.tsaxis;
       	cmgem.chwid = cmgem.txrat*cmgem.chht;
       	settextsize( cmgem.chwid, cmgem.chht );

        plcalwvtrans();

	ylinax();

   
        cmgem.lrigax = lrigaxsave;
        cmgem.llefax = llefaxsave;
        cmgem.lrigtc = lrigtcsave;
        cmgem.lleftc = lleftcsave;

        cmgem.lygrd  = lygrdsave;
        cmgem.lydiv = llydivsave;
        cmgem.lnydiv = llnydivsave;
        cmgem.nydiv = nydivsave;

        cmgem.lylab = lylabsave;
        cmgem.ltitl = ltitlsave;

	cmgem.ypmx = ypmxsave;
        cmgem.ypmn = ypmnsave;
        cmgem.xpmx = xpmxsave;
        cmgem.xpmn = xpmnsave;

        cmgem.ypmnu = ypmnusave;
        cmgem.ypmxu = ypmxusave;
        cmgem.xpmnu = xpmnusave;

        cmgem.lylim = lylimsave;
        cmgem.yimnz = yimnzsave;
        cmgem.yimxz = yimxzsave;

        
L_8888:
	return;
} /* end of function */

