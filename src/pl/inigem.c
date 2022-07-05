#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ inigem()
{
	int j, j_;
	float tatitl, taxlab, taylab;



	/*=====================================================================
	 * PURPOSE:  Variable initialization of Graphic Environment common.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/4
	 *=====================================================================
	 * PARAMETERS:
	 *    MCPTXT:  Maximum number of characters in labels, titles, etc. [i]
	 *    MPLAB:   Maximum number of auxiliary plot labels. [i]
	 *    MILINE:  Maximum number of entries in linestyle list. [i]
	 *    MISYM:   Maximum number of entries in symbol list. [i]
	 *    MICOL:   Maximum number of entries in color list. [i]
	 *    MTXSIZ:  Number of fixed text sizes. [i]
	 *=====================================================================
	 * SPECIAL NOTE:  Unless explicitely stated otherwise, for each variable
	 *                defined below that applies to the x axis, there is a
	 *                variable with a similiar name and definition which
	 *                applies to the y axis.
	 *=====================================================================
	 * VARIABLE DEFINITIONS FOR:  attribute constants.
	 *    ILIN:    Used to denote linear interpolation. [i]
	 *    ILOG:    Used to denote logarithmic interpolation. [i]
	 *    ISOLID:  Used to denote solid linestyle. [i]
	 *    ITHIN:   Used to denote thin line-width.
	 *    IDOT:    Used to denote dotted linestyle. [i]
	 *    KTXORI:  Names of allowed text orientations. [k]
	 *    VERT:    Used to denote vertical text orientation. [f]
	 *    HORZ:    Used to denote horizontal text orientation. [f]
	 *    TXSIZ:   Text sizes.  These are the heights of each
	 *             text size in plot coordinates. [f]
	 *    OTXSIZ:  Old set of text sizes. [f]
	 *    DTXSIZ:  New or default set of text sizes. [f]
	 *    TSDEF:   Default text size. [f]
	 *    THWRAT:  Default character height to width ratio. [f]
	 *    KTXSIZ:  Names of allowed text sizes. [k]
	 *    ITINY:   Used to denote tiny text size. [i]
	 *    ISMALL:  Used to denote small text size. [i]
	 *    IMED:    Used to denote medium text size. [i]
	 *    ILARGE:  Used to denote large text size. [i]
	 *    KSIDES:  Names of allowed locations for titles, labels, etc. [k]
	 *    ITOP:    Used to denote top of viewport. [i]
	 *    IBOT:    Used to denote bottom of viewport. [i]
	 *    IRIGHT:  Used to denote right of viewport. [i]
	 *    ILEFT:   Used to denote left of viewport. [i]
	 *===================================================================== */
	/* PROCEDURE: */
	cmgem.ilin = 0;
	cmgem.ilog = 1;
	cmgem.isolid = 1;
	cmgem.ithin = 1;
	cmgem.idot = 2;
	strcpy( kmgem.ktxori[0], "HORZ    " );
	strcpy( kmgem.ktxori[1], "VERT    " );
	cmgem.vert = 90.;
	cmgem.horz = 0.;
	Dtxsiz[1] = 0.015;
	Dtxsiz[2] = 0.020;
	Dtxsiz[3] = 0.030;
	Dtxsiz[4] = 0.040;
	cmgem.dtxrat = 0.6667;
	Otxsiz[1] = 0.01046;
	Otxsiz[2] = 0.01379;
	Otxsiz[3] = 0.01739;
	Otxsiz[4] = 0.02818;
	cmgem.otxrat = 1.0;
	for( j = 1; j <= MTXSIZ; j++ ){
		j_ = j - 1;
		Txsiz[j] = Dtxsiz[j];
		}
	cmgem.txrat = cmgem.dtxrat;
	cmgem.tsdef = Txsiz[2];
	strcpy( kmgem.ktxsiz[0], "TINY    " );
	strcpy( kmgem.ktxsiz[1], "SMALL   " );
	strcpy( kmgem.ktxsiz[2], "MEDIUM  " );
	strcpy( kmgem.ktxsiz[3], "LARGE   " );
	cmgem.itiny = 1;
	cmgem.ismall = 2;
	cmgem.imed = 3;
	cmgem.ilarge = 4;
	strcpy( kmgem.ksides[0], "TOP     " );
	strcpy( kmgem.ksides[1], "BOTTOM  " );
	strcpy( kmgem.ksides[2], "RIGHT   " );
	strcpy( kmgem.ksides[3], "LEFT    " );
	cmgem.itop = 1;
	cmgem.ibot = 2;
	cmgem.iright = 3;
	cmgem.ileft = 4;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  text attributes.
	 *    CHWID:   Current character width. [f]
	 *    CHHT:    Current character height. [f]
	 *    CHGAP:   Current gap between characters. [f]
	 *    TSCUR:   Current character size. [f]
	 *    KGTQUA:  Current graphics text quality. [k]
	 *             = 'HARDWARE' for hardware text.
	 *             = 'SOFTWARE' for software text.
	 *    IGTFNT:  Current graphics text font number. [i]
	 *    KHJUST:  Horizontal text justification. [k]
	 *             = 'LEFT' for left justification.
	 *             = 'CENTER' for center justification.
	 *             = 'RIGHT' for right justification.
	 *    KVJUST:  Vertical text justification. [k]
	 *             = 'BOTTOM' for bottom justification.
	 *             = 'CENTER' for center justification.
	 *             = 'TOP' for top justification.
	 *    KPTXT:   Internal file for encoding plot text. [c132]
	 *===================================================================== */

	cmgem.tscur = cmgem.tsdef;
	strcpy( kmgem.kgtqua, "SOFTWARE" );
	settexttype( kmgem.kgtqua );
	cmgem.igtfnt = 1;
	strcpy( kmgem.khjust, "LEFT    " );
	strcpy( kmgem.kvjust, "BOTTOM  " );

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  input (world) and plot (viewport) limits.
	 *    LXLIM:   Fixed limits if .TRUE., scaled to data if .FALSE. [l]
	 *    XIMN:    Minimum x limit when LXLIM is .TRUE. [f]
	 *    XIMX:    Maximum x limit when LXLIM is .TRUE. [f]
	 *    XIMNU:   Minimum x limit used. [f]
	 *    XIMXU:   Maximum x limit used. [f]
	 *    XIMNZ:   Used to compute input to plot mapping. [f]
	 *             Same as XIMNU unless logarithmic interpolation is on.
	 *    XIMXZ:   See XIMNZ.
	 *    XPMN:    Minimum x axis plot (viewport) limit. [f]
	 *             Plot coordiate space is 0. to 1. with origin
	 *             in lower left hand corner.
	 *    XPMX:    Maximum x axis plot limit. [f]
	 *    XPMNU:   Actual minimum plot coordinate used. [f]
	 *    XPMXU:   Actual maximum plot coordinate used. [f]
	 *    VSPREQ:  Requested viewspace (y to x) ratio. [f]
	 *    XVSPMN:  Minimum x viewspace value. [f;=XVSP(1)]
	 *    XVSPMN:  Maximum x viewspace value. [f;=XVSP(2)]
	 *    YVSPMN:  Minimum y viewspace value. [f;=YVSP(1)]
	 *    YVSPMN:  Maximum y viewspace value. [f;=YVSP(2)]
	 *===================================================================== */

	cmgem.lxlim = FALSE;
	cmgem.ximn = 0.;
	cmgem.ximx = 1.;
	cmgem.xpmn = 0.1;
	cmgem.xpmx = 0.9;
	cmgem.lylim = FALSE;
	cmgem.yimn = 0.;
	cmgem.yimx = 1.;
	cmgem.ypmn = 0.15;
	cmgem.ypmx = 0.9;
	setworld( cmgem.ximn, cmgem.ximx, cmgem.ximn, cmgem.ximx );
	setvport( cmgem.xpmn, cmgem.xpmx, cmgem.ypmn, cmgem.ypmx );
	cmgem.vspreq = 0.75;
	cmgem.xvspmn = 0.;
	cmgem.xvspmx = 1.;
	cmgem.yvspmn = 0.;
	cmgem.yvspmx = 0.75;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  input to plot mapping transformations.
	 *    XMPIP1:  X axis input to plot coordinate scale factor. [f]
	 *    XMPIP2:  X axis input to plot coordinate offset. [f]
	 *             Equation: XP = XMPIP1 * XI + XMPIP2 */

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  axes annotation generation attributes.
	 *    LXFUDG:  X axis limits enlarged slightly when .TRUE. [l]
	 *    XFUDG:   Fraction used to increase x axis limits by. [f]
	 *    LXREV:   Reverse x axis limits when .TRUE. [l]
	 *    IXINT:   X axis interpolation mode. [i]
	 *             = ILIN if linear interpolation is to be used.
	 *             = ILOG if logarithmic interpolation is to be used.
	 *    LXFULL:  Force full logarithmic decades when .TRUE. [l]
	 *    LLOGLB:  Force secondary log axis labeling when .TRUE. [l]
	 *    LXDIV:   Force a specific division spacing when .TRUE. [l]
	 *    XDIV:    Division spacing to use when LXDIV is .TRUE. [f]
	 *    LNXDIV:  Force a specific number of divisions when .TRUE. [l]
	 *    NXDIV:   Number of divisions to use when LNXDIV is .TRUE. [i]
	 *===================================================================== */

	cmgem.lxfudg = TRUE;
	cmgem.xfudg = 0.03;
	cmgem.lyfudg = TRUE;
	cmgem.yfudg = 0.03;
	cmgem.lxrev = FALSE;
	cmgem.lyrev = FALSE;
	cmgem.ixint = cmgem.ilin;
	cmgem.iyint = cmgem.ilin;
	cmgem.lxfull = TRUE;
	cmgem.lyfull = TRUE;
	cmgem.lloglb = TRUE;
	cmgem.lxdiv = FALSE;
	cmgem.xdiv = 1.;
	cmgem.lydiv = FALSE;
	cmgem.ydiv = 1.;
	cmgem.lnxdiv = FALSE;
	cmgem.nxdiv = 10;
	cmgem.lnydiv = FALSE;
	cmgem.nydiv = 10;
	cmgem.lxpowr = TRUE;
	cmgem.lypowr = TRUE;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  axes label attributes.
	 *    LTOPAX:  Place an annotated axes at top of plot window if .TRUE. [l]
	 *    LBOTAX:  Place an annotated axes at bottom of window if .TRUE. [l]
	 *    LRIGAX:  Place an annotated axes to right of window if .TRUE. [l]
	 *    LLEFAX:  Place an annotated axes to left of window if .TRUE. [l]
	 *    LTOPTC:  Place an axes with tick marks at top of window if .TRUE.
	 *    LBOTTC:  Place an axes with tick marks at bottom of window if .TRUE.
	 *    LRIGTC:  Place an axes with tick marks to right of window if .TRUE.
	 *    LLEFTC:  Place an axes with tick marks to left of window if .TRUE.
	 *===================================================================== */

	cmgem.ltopax = FALSE;
	cmgem.lbotax = TRUE;
	cmgem.lrigax = FALSE;
	cmgem.llefax = TRUE;
	cmgem.ltoptc = TRUE;
	cmgem.lbottc = TRUE;
	cmgem.lrigtc = TRUE;
	cmgem.lleftc = TRUE;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  data generation attributes.
	 *    LXGEN:   Generate x axis data if .TRUE. [l]
	 *    XFIRST:  First x axis data value if LXGEN is .TRUE. [f]
	 *    XDELTA:  Increment between x axis data values if LXGEN is .TRUE. [f] */

	cmgem.lxgen = FALSE;
	cmgem.xfirst = 0.;
	cmgem.xdelta = 1.;
	cmgem.lygen = FALSE;
	cmgem.yfirst = 0.;
	cmgem.ydelta = 1.;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  axes width parameters.
	 *    AXWTOP:  Current width of top axis.  This includes primary and
	 *             secondary axis annotations and any axis label.
	 *             Updated by each routine that works with axis.
	 *    AXWBOT:  Current width of bottom axis.
	 *    AXWRIG:  Current width of right axis.
	 *    AXWLEF:  Current width of left axis.
	 *    TSAXIS:  Text size of axis annotation.  See TXSIZ below.
	 *===================================================================== */

	cmgem.axwtop = 0.;
	cmgem.axwbot = 0.;
	cmgem.axwrig = 0.;
	cmgem.axwlef = 0.;
	cmgem.tsaxis = cmgem.tsdef;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  data rendering attributes.
	 *    LFLOOR:  Puts a minimum value on data when .TRUE. and
	 *             logarithmic interpolation is on. [l]
	 *    FLOOR:   Minimum data value when LFLOOR is .TRUE. [f]
	 *    LFLUSD:  Set to .TRUE. if floor was used in last data set. [l]
	 *    LRQCLP:  Viewport clipping is on if .TRUE. [l]
	 *    LTQDP:   Terminal "Quick and dirty plotting" option on if .TRUE. [l]
	 *             Data is automatically desampled before plotting when on.
	 *    NTQDP:   Approx. number of points to plot when LTQDP is .TRUE. [i]
	 *    LFQDP:   SGF "Quick and dirty plotting" option on if .TRUE. [l]
	 *             Data is automatically desampled before plotting when on.
	 *    NFQDP:   Approx. number of points to plot when LFQDP is .TRUE. [i]
	 *    LNULL:   Set to .TRUE. if want to skip null values on a plot. [i]
	 *    VNULL:   Default NULL data value. [r]
	 *===================================================================== */

	cmgem.lfloor = TRUE;
	cmgem.floor = 1.0e-10;
	cmgem.lflusd = FALSE;
	cmgem.lrqclp = FALSE;
	cmgem.ltqdp = TRUE;
	cmgem.ntqdp = 500;
	cmgem.lfqdp = TRUE;
	cmgem.nfqdp = 1000;
	cmgem.lnull = FALSE;
	cmgem.vnull = 0.0;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  grid information
	 *    LBDR:    A border is drawn around viewport when .TRUE. [l]
	 *    LXGRD:   Grid lines parallel to x axis drawn when .TRUE. [l]
	 *    IXGRD:   X axis grid linestyle. [i]
	 *===================================================================== */

	cmgem.lbdr = FALSE;
	cmgem.lxgrd = FALSE;
	cmgem.ixgrd = cmgem.idot;
	cmgem.lygrd = FALSE;
	cmgem.iygrd = cmgem.idot;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  title and label attributes.
	 *    LTITL:   Title is plotted when .TRUE. [l]
	 *    KTITL:   Text of title. [c132]
	 *    ITITLP:  Position of title. [i]
	 *    NTITL:   Number of characters in title. [i]
	 *    TSTITL:  Text size of title. [f]
	 *    TATITL:  Text orientation in degrees ccw from x. [f]
	 *    LXLAB, KXLAB, NXLAB, IXLABP, TSXLAB, TAXLAB:
	 *             X axis label info.  See definitions for LTITL, etc.
	 *    LYLAB, KYLAB, NYLAB, IYLABP, TSYLAB, TAYLAB:
	 *             Y axis label info.  See definitions for LTITL, etc.
	 *===================================================================== */

	cmgem.ltitl = FALSE;
	fstrncpy( kmgem.ktitl, 144, " ", 1 );
	cmgem.ntitl = 0;
	cmgem.ititlp = cmgem.itop;
	cmgem.tstitl = cmgem.tsdef;
	tatitl = cmgem.horz;
	cmgem.lxlab = FALSE;
	fstrncpy( kmgem.kxlab, 144, " ", 1 );
	cmgem.nxlab = 0;
	cmgem.ixlabp = cmgem.ibot;
	cmgem.tsxlab = cmgem.tsdef;
	taxlab = cmgem.horz;
	cmgem.lylab = FALSE;
	fstrncpy( kmgem.kylab, 144, " ", 1 );
	cmgem.nylab = 0;
	cmgem.iylabp = cmgem.ileft;
	cmgem.tsylab = cmgem.tsdef;
	taylab = cmgem.vert;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  plot label attributes.
	 *    NPLAB:      Number of last label modified. [i]
	 *    LPLAB(n):   Label "n" is to be plotted if .TRUE. [l]
	 *    LPLABL(n):  Label "n" to be position below label "n-1" if .TRUE. [l]
	 *                Label "n" to be placed at XPLABL(n),YPLABL(n) if .FALSE.
	 *    XPLABL(n):  X location of label "n". [f]
	 *    TSPLAB(n):  Text size of label "n". [f]
	 *    TAPLAB(n):  Text position of label "n". [f]
	 *    KPLAB(n):   Text of label "n". [c132]
	 *===================================================================== */

	cmgem.nplab = 0;
	cmgem.tsplab[0] = cmgem.tsdef;
	cmgem.taplab[0] = cmgem.horz;
	cmgem.xplabl[0] = 0.15;
	cmgem.yplabl[0] = 0.20;
	for( j = 1; j <= MPLAB; j++ ){
		j_ = j - 1;
		Lplab[j] = FALSE;
		cmgem.tsplab[j] = cmgem.tsdef;
		cmgem.taplab[j] = cmgem.horz;
		fstrncpy( kmgem.kplab[j_], 144, " ", 1 );
		Lplabl[j] = TRUE;
		}
	cmgem.xplabl[1] = 0.15;
	cmgem.yplabl[1] = 0.20;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  line drawing attributes.
	 *    LLINE:      Line drawing is on if .TRUE. [l]
	 *    ICLINE:     Current linestyle. [i]
	 *    LILINE:     Automatic linestyle incrementing on if .TRUE. [i]
	 *                Incrementing done for each new data array.
	 *    IILINE:     Array of linestyles for automatic incrementing. [i]
	 *    NILINE:     Current lenght of IILINE. [i]
	 *    JILINE:     Current pointer in IILINE. [i]
	 *    ISKLIN:     Skeleton linestyle. [i]
	 *                The skeleton includes axes lines, grid lines, etc.
	 *===================================================================== */

	cmgem.lline = TRUE;
	inilin( cmgem.iiline, &cmgem.niline );
	cmgem.liline = FALSE;
	cmgem.jiline = 1;
	cmgem.icline = Iiline[cmgem.jiline];
	cmgem.isklin = cmgem.isolid;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  symbol drawing attributes.
	 *    LSYM:       Symbol drawing is on if .TRUE. [l]
	 *    ISYM:       Current symbol number. [i]
	 *    LNWSYM:     Set to .TRUE. if symbol number has been changed. [l]
	 *    LISYM:      Automatic symbol number incrementing on if .TRUE. [i]
	 *                Incrementing done for each new data array.
	 *    IISYM:      Array of symbol numbers for automatic incrementing. [i]
	 *    NISYM:      Current lenght of IISYM. [i]
	 *    JISYM:      Current pointer in IISYM. [i]
	 *    SYMSZ:      Symbol size in world coordinates. [f]
	 *    SYMSP:      Minimum symbol spacing in world coordinates. [f]
	 *===================================================================== */

	cmgem.lsym = FALSE;
	inisym( cmgem.iisym, &cmgem.nisym );
	cmgem.lisym = FALSE;
	cmgem.jisym = 1;
	cmgem.lnwsym = TRUE;
	cmgem.symsz = 0.01;
	cmgem.symsp = 0.0;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  line-width attributes.
	 *    LWIDTH:       Width drawing is on if .TRUE. [l]
	 *    IWIDTH:       Current width number. [i]
	 *    LIWIDTH:      Automatic width number incrementing on if .TRUE. [i]
	 *    IIWIDTH:      Array of width numbers for automatic incrementing. [i]
	 *    NIWIDTH:      Current lenght of IIWIDTH. [i]
	 *    JIWIDTH:      Current pointer in IIWIDTHL. [i]
	 *    ISKWIDTH:     Current sleleton width value. [i]
	 *    SKDEVFUDGE:   Fudge factor for skeleton (axis) line width. [f]
	 *===================================================================== */

	cmgem.jiwidth = 1;
	cmgem.iwidth = cmgem.ithin;
	cmgem.iswidth = cmgem.ithin;
        cmgem.isymwidth = cmgem.ithin;
	cmgem.iskwidth = cmgem.ithin;
	cmgem.isskwidth = cmgem.ithin;
	cmgem.skdevfudge = 1;
	iniwidth();
	cmgem.lwidth = FALSE;
	cmgem.liwidth = FALSE;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  color attributes.
	 *    LCOL:       Color drawing is on if .TRUE. [l]
	 *    ICOL:       Current color number. [i]
	 *    LICOL:      Automatic color number incrementing on if .TRUE. [i]
	 *    IICOL:      Array of color numbers for automatic incrementing. [i]
	 *    NICOL:      Current lenght of IICOL. [i]
	 *    JICOL:      Current pointer in IICOL. [i]
	 *    ISKCOL:     Skeleton color number. [i]
	 *    IBACOL:     Background color number. [i]
	 *===================================================================== */

	cmgem.lcol = FALSE;
	inicol( cmgem.iicol, &cmgem.nicol );
	cmgem.licol = FALSE;
	cmgem.jicol = 1;
	cmgem.icol = MLARGE;
	cmgem.iskcol = MLARGE;
	cmgem.ibacol = 0;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  automatic framing flag.
	 *    LFRAME:  Automatic framing between data sets on if .TRUE. [l]
	 *===================================================================== */

	cmgem.lframe = TRUE;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  expansion area.
	 *    EXTGEM:  Used to store new variables between major updates. [f]
	 *    KXTGEM:  Used to store new character variables between updates.
	 *===================================================================== */

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  save/restore variables.
	 *    CMGEMA:  Location of beginning of common block CMGEM.
	 *    KMGEMA:  Location of beginning of common block KMGEM.
	 *    LGEMS:   Graphics environment has been saved if .TRUE. [l]
	 *    NCMGEM:  Length of common block CMGEM to save. [i]
	 *    NKMGEM:  Length of common block KMGEM to save. [i]
	 *    CMGEMS:  Area used to save CMGEM. [f]
	 *    KMGEMS:  Area used to save KMGEM. [k]
	 *===================================================================== */

	lgems = FALSE;

	/*=====================================================================
	 * VARIABLE DEFINITIONS FOR:  annotation generation constants.
	 *    FAC:     Fractions used to compute logarithmic scalings. [f]
	 *    KFAC:    Integers 1 through 9 used in logarithmic labeling. [c1]
	 *===================================================================== */

	Fac[1] = 0.0000;
	Fac[2] = 0.3010;
	Fac[3] = 0.4771;
	Fac[4] = 0.6021;
	Fac[5] = 0.6990;
	Fac[6] = 0.7782;
	Fac[7] = 0.8451;
	Fac[8] = 0.9031;
	Fac[9] = 0.9542;
	Kfac[1] = '1';
	Kfac[2] = '2';
	Kfac[3] = '3';
	Kfac[4] = '4';
	Kfac[5] = '5';
	Kfac[6] = '6';
	Kfac[7] = '7';
	Kfac[8] = '8';
	Kfac[9] = '9';

	/* lprint is TRUE when the user wishes to print a plot, FALSE by default 
	   lSGFtemp is TRUE when SGF is turned on exclusively for PRINT option.
	   kptrName is the name of the printer to which to print. */

	cmgem.lprint = FALSE ;
	cmgem.lSGFtemp = FALSE ;
	kmgem.kptrName[0] = '\0' ;

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    920527:  Added line-width initialization.
	 *    910301:  Changed iline to icline.
	 *    900529:  Deleted call to setsymbolnum. Fixed VAX/VMS problem.
	 *    900430:  Fixed bug in initial PLABEL position.
	 *    860213:  Deleted LNICE initialization.
	 *    850610:  Modified symbol initialization logic.
	 *    850510:  Changed default color value to 0.
	 *    850211:  Changed default value of text width/height ratio.
	 *    841030:  Changed default text sizes.  Old sizes can be requested.
	 *    841018:  Added text justification constants initialization.
	 *    831005:  Documented GEM common block.
	 *    820324:  Changed initial values of titles and labels to medium.
	 *    820305:  Added color attribute initialization.
	 *    811104:  Changed initial value of LTITL to .FALSE.
	 *    810928:  Added text quality and font attribute initialization.
	 *    810722:  Added symbol attribute initilization.
	 *    810414:  Original version.
	 *===================================================================== */

} /* end of function */

