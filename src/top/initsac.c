


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define DOINITS
#include "../../inc/mach.h"
#include "../../inc/bbs.h"
#include "../../inc/bom.h"
#include "../../inc/cnd.h"
#include "../../inc/cnv.h"
#include "../../inc/com.h"
#include "../../inc/comlists.h"
#include "../../inc/contouring.h"
#include "../../inc/cpf.h"
#include "../../inc/csf.h"
#include "../../inc/datafilelist.h"
#include "../../inc/dfir.h"
#include "../../inc/dfm.h"
#include "../../inc/dload.h"
#include "../../inc/eam.h"
#include "../../inc/exm.h"
#include "../../inc/fir.h"
#include "../../inc/fks.h"
#include "../../inc/gam.h"
#include "../../inc/gd2.h"
#include "../../inc/gdm.h"
#include "../../inc/gem.h"
#include "../../inc/hdr.h"
#include "../../inc/icm.h"
#include "../../inc/lhf.h"
#include "../../inc/mem.h"
#include "../../inc/msg.h"
#include "../../inc/nnm.h"
#include "../../inc/nvars.h"
#include "../../inc/sam.h"
#include "../../inc/scm.h"
#include "../../inc/sddhdr.h"
#include "../../inc/site.h"
#include "../../inc/smm.h"
#include "../../inc/snf.h"
#include "../../inc/spe.h"
#include "../../inc/specdata.h"
#include "../../inc/spectrogram.h"
#include "../../inc/tok.h"
#include "../../inc/tt.h"
#include "../../inc/uom.h"
#include "../../inc/usr.h"
#include "../../inc/vars.h"
#include "../../inc/wild.h"
#undef DOINITS
void xabout ( void );
void /*FUNCTION*/ initsac(void)
{
	int nerr;
	void zgetgd(), zinfo();
        char *dummy;
        int i;
        static int ifirst = 1;

	/*=====================================================================
	 * PURPOSE:  To initialize (or reinitialize) SAC.
	 *=====================================================================
	 * MODULE/LEVEL:  exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MUNOUT
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    exm:     ktime, kdate, kmach
	 *    gam:     kgddef
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  inimsg, initcommon, begingraphics, zinfo, flash, zgetgd,
	 *             setmsg, apcmsg, aplmsg
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920330:  Block data initialization implemented during proting to 
	 *             IBM RISC 6000.
	 *    890105:  Changed from terminal output to message subsystem.
	 *    881122:  Added initialization of default graphics device.
	 *    860327:  Moved call to DELIMS from here to INICM.
	 *    860218:  Added call to INIMSG.
	 *    830818:  Added call to ZGTERM.
	 *    821004:  Added initialization of graphics library.
	 *    810429:  Original version from top of MAINLP.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850222
	 *===================================================================== */
	/* PROCEDURE: */
/*
#ifndef POSIX
        i = ieee_flags("set","direction","nearest",&dummy);
#endif
*/
	/* -- BLOCK DATA initialization of variables in common blocks. */
	initblkdata();

	/* - Initialization that can't be handled in lower level modules. */

	/* -- SAC error message function. */
	inimsg();

	/* -- Common blocks. */
	initcommon();

	/* -- Graphics Library. */
	begingraphics( &nerr );

	/* -- Create time/date/machine stamp. */
	zinfo( kmexm.ktime,9, kmexm.kdate,9, kmexm.kmach,9 );

	/* -- Get name of default graphics device. */
	zgetgd( kmgam.kgddef,9 );

	/* - Say hello. */

        if (ifirst){
            ifirst = 0;
	    xabout () ;
        }

	/* Initialize Data Base Module */
	inissi () ;

L_8888:
	return;

} /* end of function */




void /*FUNCTION*/ initblkdata()
{
        int _i, _r;
        static int _aini = 1;


        if( _aini ){ /* Do 1 TIME INITIALIZATIONS! */
                cmextcom.nfiles = 0;
                cmgdm.lginit = FALSE;
		cmdfm.ndsflcnt = 0;
                cmicnv.icnver = 0;
                cmvars.lvarsinit = FALSE;
                cmgetvlist.nlevelsgt = 0;
                cmcopyvlist.nlevelscp = 0;
                cmprintvlist.nlevelspr = 0;
                strcpy( kmvars.varsidcode, "VARS" );
                _aini = 0;
        }

        /* - inc/dload */
        /* - inc/gdm */
        /* - inc/dfm */
        /* - inc/cnv */
        /* - inc/vars */
    
        a = (float*)((char*)cmhdr.fhdr + 32);
        arrivl = (float*)((char*)cmhdr.fhdr + 32);
        az = (float*)((char*)cmhdr.fhdr + 204);
        b = (float*)((char*)cmhdr.fhdr + 20);
        baz = (float*)((char*)cmhdr.fhdr + 208);
        begin = (float*)((char*)cmhdr.fhdr + 20);
        cmpaz = (float*)((char*)cmhdr.fhdr + 228);
        cmpinc = (float*)((char*)cmhdr.fhdr + 232);
        delta = (float*)cmhdr.fhdr;
        depmax = (float*)((char*)cmhdr.fhdr + 8);
        depmen = (float*)((char*)cmhdr.fhdr + 224);
        depmin = (float*)((char*)cmhdr.fhdr + 4);
        depmn = (float*)((char*)cmhdr.fhdr + 4);
        depmx = (float*)((char*)cmhdr.fhdr + 8);
        dist = (float*)((char*)cmhdr.fhdr + 200);
        e = (float*)((char*)cmhdr.fhdr + 24);
        ennd = (float*)((char*)cmhdr.fhdr + 24);
        evdp = (float*)((char*)cmhdr.fhdr + 152);
        evel = (float*)((char*)cmhdr.fhdr + 148);
        evla = (float*)((char*)cmhdr.fhdr + 140);
        evlo = (float*)((char*)cmhdr.fhdr + 144);
        f = (float*)((char*)cmhdr.fhdr + 80);
        mag = (float*)((char*)cmhdr.fhdr + 156);   /* magnitude. maf 970205 */
        fhdr64 = (float*)((char*)cmhdr.fhdr + 252);
        fhdr65 = (float*)((char*)cmhdr.fhdr + 256);
        fhdr66 = (float*)((char*)cmhdr.fhdr + 260);
        fhdr67 = (float*)((char*)cmhdr.fhdr + 264);
        fhdr68 = (float*)((char*)cmhdr.fhdr + 268);
        fhdr69 = (float*)((char*)cmhdr.fhdr + 272);
        fhdr70 = (float*)((char*)cmhdr.fhdr + 276);
        fini = (float*)((char*)cmhdr.fhdr + 80);
        fmean = (float*)((char*)cmhdr.fhdr + 224);
        fmt = (float*)((char*)cmhdr.fhdr + 36);
        gcarc = (float*)((char*)cmhdr.fhdr + 212);
        ia = (int*)((char*)cmhdr.niv + 44);
        iacc = (int*)((char*)cmhdr.niv + 28);
        iamph = (int*)((char*)cmhdr.niv + 8);
        ib = (int*)((char*)cmhdr.niv + 32);
        ichem = (int*)((char*)cmhdr.niv + 168);
        iday = (int*)((char*)cmhdr.niv + 36);
        idep = (int*)((char*)cmhdr.ihdr + 4);
 	    idisp = (int*)((char*)cmhdr.niv + 20);
	    idown = (int*)((char*)cmhdr.niv + 116);
	    idrop = (int*)((char*)cmhdr.niv + 184);
	    ieast = (int*)((char*)cmhdr.niv + 108);
	    ievreg = (int*)((char*)cmhdr.ihdr + 24);
	    ievtyp = (int*)((char*)cmhdr.ihdr + 28);
	    iftype = (int*)cmhdr.ihdr;
	    iglch = (int*)((char*)cmhdr.niv + 180);
	    igood = (int*)((char*)cmhdr.niv + 176);
	    imagtyp = (int*)((char*)cmhdr.ihdr + 40);/* magnitude type. maf */
	    imagsrc = (int*)((char*)cmhdr.ihdr + 44);/* magnitude source. 970205 */
	    ihdr13 = (int*)((char*)cmhdr.ihdr + 48);
	    ihdr14 = (int*)((char*)cmhdr.ihdr + 52);
	    ihdr15 = (int*)((char*)cmhdr.ihdr + 56);
	    ihdr16 = (int*)((char*)cmhdr.ihdr + 60);
	    ihdr17 = (int*)((char*)cmhdr.ihdr + 64);
	    ihdr18 = (int*)((char*)cmhdr.ihdr + 68);
	    ihdr19 = (int*)((char*)cmhdr.ihdr + 72);
	    ihdr20 = (int*)((char*)cmhdr.ihdr + 76);
	    ihdr4 = (int*)((char*)cmhdr.ihdr + 12);
	    ihglp = (int*)((char*)cmhdr.niv + 136);
	    ihorza = (int*)((char*)cmhdr.niv + 112);
	    iinst = (int*)((char*)cmhdr.ihdr + 16);
	    illlbb = (int*)((char*)cmhdr.niv + 124);
	    ilowsn = (int*)((char*)cmhdr.niv + 188);
    /* these 18 added for magnitude info. maf 970205 */
        imb = (int*)((char*)cmhdr.niv + 204);
        ims = (int*)((char*)cmhdr.niv + 208);
        iml = (int*)((char*)cmhdr.niv + 212);
        imw = (int*)((char*)cmhdr.niv + 216);
        imd = (int*)((char*)cmhdr.niv + 220);
        imx = (int*)((char*)cmhdr.niv + 224);
        ineic = (int*)((char*)cmhdr.niv + 228);
        ipdeq = (int*)((char*)cmhdr.niv + 232);
	    ipdew = (int*)((char*)cmhdr.niv + 236);
	    ipde = (int*)((char*)cmhdr.niv + 240);
        iisc = (int*)((char*)cmhdr.niv + 244);
        ireb = (int*)((char*)cmhdr.niv + 248);
        iusgs = (int*)((char*)cmhdr.niv + 252);
        ibrk = (int*)((char*)cmhdr.niv + 256);
        icaltech = (int*)((char*)cmhdr.niv + 260);
        illnl = (int*)((char*)cmhdr.niv + 264);
        ievloc = (int*)((char*)cmhdr.niv + 268);
        ijsop = (int*)((char*)cmhdr.niv + 272);
        iuser = (int*)((char*)cmhdr.niv + 276);
        iunknown = (int*)((char*)cmhdr.niv + 280);
	/* These 17 lines added for ievtyp.  maf 970325 */
	    iqb = (int*)((char*)cmhdr.niv + 284);
	    iqb1 = (int*)((char*)cmhdr.niv + 288);
	    iqb2 = (int*)((char*)cmhdr.niv + 292);
        iqbx = (int*)((char*)cmhdr.niv + 296);
        iqmt = (int*)((char*)cmhdr.niv + 300);
        ieq = (int*)((char*)cmhdr.niv + 304);
        ieq1 = (int*)((char*)cmhdr.niv + 308);
        ieq2 = (int*)((char*)cmhdr.niv + 312);
        ime = (int*)((char*)cmhdr.niv + 316);
        iex = (int*)((char*)cmhdr.niv + 320);
        inu = (int*)((char*)cmhdr.niv + 324);
        inc = (int*)((char*)cmhdr.niv + 328);
        io_ = (int*)((char*)cmhdr.niv + 332);
        il = (int*)((char*)cmhdr.niv + 336);
        ir = (int*)((char*)cmhdr.niv + 340);
        it = (int*)((char*)cmhdr.niv + 344);
        iu = (int*)((char*)cmhdr.niv + 348);
    /* These 9 added for ievtyp, to keep up with database. maf 000530 */
        ieq3 = (int*)((char*)cmhdr.niv + 352);
        ieq0 = (int*)((char*)cmhdr.niv + 356);
        iex0 = (int*)((char*)cmhdr.niv + 360);
        iqc = (int*)((char*)cmhdr.niv + 364);
        iqb0 = (int*)((char*)cmhdr.niv + 368);
        igey = (int*)((char*)cmhdr.niv + 372);
        ilit = (int*)((char*)cmhdr.niv + 376);
        imet = (int*)((char*)cmhdr.niv + 380);
        iodor = (int*)((char*)cmhdr.niv + 384);
    
	    inorth = (int*)((char*)cmhdr.niv + 104);
	    inucl = (int*)((char*)cmhdr.niv + 144);
	    io = (int*)((char*)cmhdr.niv + 40);
	    iother = (int*)((char*)cmhdr.niv + 172);
	    ipostn = (int*)((char*)cmhdr.niv + 152);
	    ipostq = (int*)((char*)cmhdr.niv + 164);
	    ipren = (int*)((char*)cmhdr.niv + 148);
	    ipreq = (int*)((char*)cmhdr.niv + 160);
	    iquake = (int*)((char*)cmhdr.niv + 156);
	    iqual = (int*)((char*)cmhdr.ihdr + 32);
	    iradev = (int*)((char*)cmhdr.niv + 96);
	    iradnv = (int*)((char*)cmhdr.niv + 88);
	    irldta = (int*)((char*)cmhdr.niv + 192);
	    irlim = (int*)((char*)cmhdr.niv + 4);
	    isro = (int*)((char*)cmhdr.niv + 140);
	    istreg = (int*)((char*)cmhdr.ihdr + 20);
	    isynth = (int*)((char*)cmhdr.ihdr + 36);
	    it0 = (int*)((char*)cmhdr.niv + 48);
	    it1 = (int*)((char*)cmhdr.niv + 52);
	    it2 = (int*)((char*)cmhdr.niv + 56);
	    it3 = (int*)((char*)cmhdr.niv + 60);
	    it4 = (int*)((char*)cmhdr.niv + 64);
	    it5 = (int*)((char*)cmhdr.niv + 68);
	    it6 = (int*)((char*)cmhdr.niv + 72);
	    it7 = (int*)((char*)cmhdr.niv + 76);
	    it8 = (int*)((char*)cmhdr.niv + 80);
	    it9 = (int*)((char*)cmhdr.niv + 84);
	    itanev = (int*)((char*)cmhdr.niv + 100);
	    itannv = (int*)((char*)cmhdr.niv + 92);
	    itime = (int*)cmhdr.niv;
	    iunkn = (int*)((char*)cmhdr.niv + 16);
	    iup = (int*)((char*)cmhdr.niv + 120);
	    ivel = (int*)((char*)cmhdr.niv + 24);
	    ivolts = (int*)((char*)cmhdr.niv + 196);
	    iwwsn1 = (int*)((char*)cmhdr.niv + 128);
	    iwwsn2 = (int*)((char*)cmhdr.niv + 132);
	    ixy = (int*)((char*)cmhdr.niv + 12);
	    ixyz = (int*)((char*)cmhdr.niv + 200);
	    iztype = (int*)((char*)cmhdr.ihdr + 8);
    
	    kstnm = (char*)kmhdr.khdr;
	    kevnm = (char*)((char*)kmhdr.khdr + 9);
	    khole = (char*)((char*)kmhdr.khdr + 27);
	    ko = (char*)((char*)kmhdr.khdr + 36);
	    ka = (char*)((char*)kmhdr.khdr + 45);
	    kt0 = (char*)((char*)kmhdr.khdr + 54);
	    kt1 = (char*)((char*)kmhdr.khdr + 63);
	    kt2 = (char*)((char*)kmhdr.khdr + 72);
	    kt3 = (char*)((char*)kmhdr.khdr + 81);
	    kt4 = (char*)((char*)kmhdr.khdr + 90);
	    kt5 = (char*)((char*)kmhdr.khdr + 99);
	    kt6 = (char*)((char*)kmhdr.khdr + 108);
	    kt7 = (char*)((char*)kmhdr.khdr + 117);
	    kt8 = (char*)((char*)kmhdr.khdr + 126);
	    kt9 = (char*)((char*)kmhdr.khdr + 135);
	    kf = (char*)((char*)kmhdr.khdr + 144);
	    kuser0 = (char*)((char*)kmhdr.khdr + 153);
	    kuser1 = (char*)((char*)kmhdr.khdr + 162);
	    kuser2 = (char*)((char*)kmhdr.khdr + 171);
	    kcmpnm = (char*)((char*)kmhdr.khdr + 180);
	    knetwk = (char*)((char*)kmhdr.khdr + 189);
	    kdatrd = (char*)((char*)kmhdr.khdr + 198);
	    kinst = (char*)((char*)kmhdr.khdr + 207);
    
	    lcalda = (int*)((char*)cmhdr.lhdr + 12);
	    leven = (int*)cmhdr.lhdr;
	    lhdr5 = (int*)((char*)cmhdr.lhdr + 16);
	    lovrok = (int*)((char*)cmhdr.lhdr + 8);
	    lpspol = (int*)((char*)cmhdr.lhdr + 4);
        nevid = (int*)((char*)cmhdr.nhdr + 32);
	    nhdr15 = (int*)((char*)cmhdr.nhdr + 56);
	    norid = (int*)((char*)cmhdr.nhdr + 28);
	    npts = (int*)((char*)cmhdr.nhdr + 36);
	    nsnpts = (int*)((char*)cmhdr.nhdr + 40);
	    nvhdr = (int*)((char*)cmhdr.nhdr + 24);
        nwfid = (int*)((char*)cmhdr.nhdr + 44);
	    nxsize = (int*)((char*)cmhdr.nhdr + 48);
	    nysize = (int*)((char*)cmhdr.nhdr + 52);
	    nzdttm = (int*)cmhdr.nhdr;
	    nzhour = (int*)((char*)cmhdr.nhdr + 8);
	    nzjday = (int*)((char*)cmhdr.nhdr + 4);
	    nzmin = (int*)((char*)cmhdr.nhdr + 12);
	    nzmsec = (int*)((char*)cmhdr.nhdr + 20);
	    nzsec = (int*)((char*)cmhdr.nhdr + 16);
	    nzyear = (int*)cmhdr.nhdr;
	    o = (float*)((char*)cmhdr.fhdr + 28);
	    odelta = (float*)((char*)cmhdr.fhdr + 16);
	    origin = (float*)((char*)cmhdr.fhdr + 28);
	    resp0 = (float*)((char*)cmhdr.fhdr + 84);
	    resp1 = (float*)((char*)cmhdr.fhdr + 88);
	    resp2 = (float*)((char*)cmhdr.fhdr + 92);
	    resp3 = (float*)((char*)cmhdr.fhdr + 96);
	    resp4 = (float*)((char*)cmhdr.fhdr + 100);
	    resp5 = (float*)((char*)cmhdr.fhdr + 104);
	    resp6 = (float*)((char*)cmhdr.fhdr + 108);
	    resp7 = (float*)((char*)cmhdr.fhdr + 112);
	    resp8 = (float*)((char*)cmhdr.fhdr + 116);
	    resp9 = (float*)((char*)cmhdr.fhdr + 120);
	    sb = (float*)((char*)cmhdr.fhdr + 216);
	    scale = (float*)((char*)cmhdr.fhdr + 12);
	    sdelta = (float*)((char*)cmhdr.fhdr + 220);
	    stdp = (float*)((char*)cmhdr.fhdr + 136);
	    stel = (float*)((char*)cmhdr.fhdr + 132);
	    stla = (float*)((char*)cmhdr.fhdr + 124);
	    stlo = (float*)((char*)cmhdr.fhdr + 128);
	    t0 = (float*)((char*)cmhdr.fhdr + 40);
	    t1 = (float*)((char*)cmhdr.fhdr + 44);
	    t2 = (float*)((char*)cmhdr.fhdr + 48);
	    t3 = (float*)((char*)cmhdr.fhdr + 52);
	    t4 = (float*)((char*)cmhdr.fhdr + 56);
	    t5 = (float*)((char*)cmhdr.fhdr + 60);
	    t6 = (float*)((char*)cmhdr.fhdr + 64);
	    t7 = (float*)((char*)cmhdr.fhdr + 68);
	    t8 = (float*)((char*)cmhdr.fhdr + 72);
	    t9 = (float*)((char*)cmhdr.fhdr + 76);
	    time0 = (float*)((char*)cmhdr.fhdr + 40);
	    time1 = (float*)((char*)cmhdr.fhdr + 44);
	    time2 = (float*)((char*)cmhdr.fhdr + 48);
	    time3 = (float*)((char*)cmhdr.fhdr + 52);
	    time4 = (float*)((char*)cmhdr.fhdr + 56);
	    time5 = (float*)((char*)cmhdr.fhdr + 60);
	    time6 = (float*)((char*)cmhdr.fhdr + 64);
	    time7 = (float*)((char*)cmhdr.fhdr + 68);
	    time8 = (float*)((char*)cmhdr.fhdr + 72);
	    time9 = (float*)((char*)cmhdr.fhdr + 76);
	    user0 = (float*)((char*)cmhdr.fhdr + 160);
	    user1 = (float*)((char*)cmhdr.fhdr + 164);
	    user2 = (float*)((char*)cmhdr.fhdr + 168);
	    user3 = (float*)((char*)cmhdr.fhdr + 172);
	    user4 = (float*)((char*)cmhdr.fhdr + 176);
	    user5 = (float*)((char*)cmhdr.fhdr + 180);
	    user6 = (float*)((char*)cmhdr.fhdr + 184);
	    user7 = (float*)((char*)cmhdr.fhdr + 188);
	    user8 = (float*)((char*)cmhdr.fhdr + 192);
	    user9 = (float*)((char*)cmhdr.fhdr + 196);
	    xmaximum = (float*)((char*)cmhdr.fhdr + 240);
	    xminimum = (float*)((char*)cmhdr.fhdr + 236);
	    ymaximum = (float*)((char*)cmhdr.fhdr + 248);
	    yminimum = (float*)((char*)cmhdr.fhdr + 244);
        swapHeaderByte = (float*)((char*)cmhdr.fhdr + 252);   
    
    
    
    /*iscalg = (int*)((char*)kmshdr.kshdr + 112);*/
    isclas = (int*)kmshdr.kshdr;
    iscom = (int*)((char*)kmshdr.kshdr + 40);
    isdate = (int*)((char*)kmshdr.kshdr + 80);
    isdelt = (int*)((char*)kmshdr.kshdr + 88);
    isfrmt = (int*)((char*)kmshdr.kshdr + 4);
    ishdr = (int*)kmshdr.kshdr;
    isnpts = (int*)((char*)kmshdr.kshdr + 92);
    isrep = (int*)((char*)kmshdr.kshdr + 116);
    issdep = (int*)((char*)kmshdr.kshdr + 108);
    issel = (int*)((char*)kmshdr.kshdr + 96);
    issla = (int*)((char*)kmshdr.kshdr + 100);
    isslo = (int*)((char*)kmshdr.kshdr + 104);
    istime = (int*)((char*)kmshdr.kshdr + 84);
    kschan = (char*)((char*)kmshdr.kshdr + 28);
    kschdr = (char*)kmshdr.kshdr;
    ksclas = (char*)kmshdr.kshdr;
    kscom = (char*)((char*)kmshdr.kshdr + 40);
    ksevnm = (char*)((char*)kmshdr.kshdr + 12);
    ksfrmt = (char*)((char*)kmshdr.kshdr + 4);
    ksstnm = (char*)((char*)kmshdr.kshdr + 20);
    
    
    /* Iscom = &iscom[0] - 1; */
    Iscom = (int*)((char*)kmshdr.kshdr + 40 - 4);
    
    /* Ishdr = &ishdr[0] - 1; */
    Ishdr = (int*)((char*)kmshdr.kshdr - 4);
    
    /* Isrep = &isrep[0] - 1; */
    Isrep = (int*)((char*)kmshdr.kshdr + 116 - 4);
    


        return ;
} /* end of function */

