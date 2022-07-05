CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c  -DBUILD_DATE="\"`date +%m/%d/20%y`\"" $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libexm.a

LIBOBJS = gettextwait.o  iniexm.o    proerr.o      qam.o         xhelp.o         xwritebbf.o \
          qapf.o         qcolor.o    qcut.o        qdevices.o    qfid.o          qgtext.o \
          qhpf.o         qline.o     qmtw.o        qpicks.o      qsymbol.o       repav.o \
          qtitle.o       qwidth.o    qxlabl.o      qxlim.o       qylabl.o        qylim.o \
          reperr.o       repiv.o     repivl.o      repkv.o       replv.o         reprtw.o \
          reprv.o        setprompt.o settextwait.o tracereport.o tracevariable.o xunsetbb.o \
          xcd.o          xecho.o     xeval.o       xexmc.o       xfg.o \
          xinstallmacr.o xload.o     xmacro.o      xmsg.o        xnews.o         xpause.o \
          xreadbbf.o     xreport.o   xsetbb.o      xsetmacro.o   xsyntx.o        xgetbb.o \
          xsystemcomma.o xtrace.o    xtranscript.o xabout.o    

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)



clean:
	/bin/rm -f *.o 

gettextwait.o:   gettextwait.c $(INC)/mach.h $(INC)/exm.h 
iniexm.o:        iniexm.c $(INC)/mach.h $(INC)/exm.h 
proerr.o:        proerr.c $(INC)/mach.h $(INC)/com.h $(INC)/exm.h 
qam.o:           qam.c $(INC)/mach.h $(INC)/mem.h 
qapf.o:          qapf.c $(INC)/mach.h $(INC)/eam.h 
qcolor.o:        qcolor.c $(INC)/mach.h $(INC)/gem.h 
qcut.o:          qcut.c $(INC)/mach.h $(INC)/dfm.h 
qdevices.o:      qdevices.c $(INC)/mach.h 
qfid.o:          qfid.c $(INC)/mach.h $(INC)/gam.h 
qgtext.o:        qgtext.c $(INC)/mach.h $(INC)/gem.h 
qhpf.o:          qhpf.c $(INC)/mach.h $(INC)/eam.h 
qline.o:         qline.c $(INC)/mach.h $(INC)/gem.h 
qmtw.o:          qmtw.c $(INC)/mach.h $(INC)/smm.h 
qpicks.o:        qpicks.c $(INC)/mach.h $(INC)/gam.h 
qsymbol.o:       qsymbol.c $(INC)/mach.h $(INC)/gem.h 
qtitle.o:        qtitle.c $(INC)/mach.h $(INC)/gem.h 
qxlabl.o:        qxlabl.c $(INC)/mach.h $(INC)/gem.h 
qxlim.o:         qxlim.c $(INC)/mach.h $(INC)/gam.h 
qylabl.o:        qylabl.c $(INC)/mach.h $(INC)/gem.h 
qylim.o:         qylim.c $(INC)/mach.h $(INC)/gam.h 
qwidth.o:        qwidth.c $(INC)/mach.h $(INC)/gem.h 
repav.o:         repav.c $(INC)/mach.h 
reperr.o:        reperr.c $(INC)/mach.h 
repiv.o:         repiv.c $(INC)/mach.h 
repivl.o:        repivl.c $(INC)/mach.h 
repkv.o:         repkv.c $(INC)/mach.h 
replv.o:         replv.c $(INC)/mach.h 
reprtw.o:        reprtw.c $(INC)/mach.h 
reprv.o:         reprv.c $(INC)/mach.h 
setprompt.o:     setprompt.c $(INC)/mach.h $(INC)/exm.h 
settextwait.o:   settextwait.c $(INC)/mach.h $(INC)/exm.h 
tracereport.o:   tracereport.c $(INC)/mach.h $(INC)/exm.h 
tracevariable.o: tracevariable.c $(INC)/mach.h $(INC)/exm.h 
xcd.o:           xcd.c $(INC)/mach.h $(INC)/exm.h
xecho.o:         xecho.c $(INC)/mach.h $(INC)/exm.h $(INC)/msg.h 
xeval.o:         xeval.c $(INC)/mach.h $(INC)/exm.h  $(INC)/cnv.h
xexmc.o:         xexmc.c $(INC)/mach.h $(INC)/exm.h 
xfg.o:           xfg.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                 $(INC)/mem.h $(INC)/exm.h 
xgetbb.o:        xgetbb.c $(INC)/mach.h $(INC)/exm.h $(INC)/bbs.h 
xhelp.o:         xhelp.c $(INC)/mach.h 
xinstallmacr.o:  xinstallmacr.c $(INC)/mach.h 
xload.o:         xload.c $(INC)/mach.h $(INC)/comlists.h 
xmacro.o:        xmacro.c $(INC)/mach.h $(INC)/exm.h 
xmsg.o:          xmsg.c $(INC)/mach.h $(INC)/com.h 
xnews.o:         xnews.c $(INC)/mach.h 
xpause.o:        xpause.c $(INC)/mach.h $(INC)/exm.h 
xreadbbf.o:      xreadbbf.c $(INC)/mach.h $(INC)/bbs.h 
xreport.o:       xreport.c $(INC)/mach.h $(INC)/exm.h 
xsetbb.o:        xsetbb.c $(INC)/mach.h 
xsetmacro.o:     xsetmacro.c $(INC)/mach.h $(INC)/exm.h 
xsyntx.o:        xsyntx.c $(INC)/mach.h $(INC)/exm.h 
xsystemcomma.o:  xsystemcomma.c $(INC)/mach.h 
xtrace.o:        xtrace.c $(INC)/mach.h $(INC)/exm.h 
xtranscript.o:   xtranscript.c $(INC)/mach.h $(INC)/exm.h $(INC)/msg.h 
xunsetbb.o:      xunsetbb.c $(INC)/mach.h 
xwritebbf.o:     xwritebbf.c $(INC)/mach.h $(INC)/bbs.h 


