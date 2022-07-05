      program sgfplot
*=====================================================================
* PURPOSE:  To plot SAC Graphics Files (SGF) to another device.
*=====================================================================
* MODULE/LEVEL:  sgf/1
*=====================================================================
* GLOBAL INPUT:
*    mach:    mcmsg, mcpfn, munout
*=====================================================================
* SUBROUTINES CALLED:
*    local:   plotfile
*    saclib:  zgimsg, zgpmsg, indexb, poptok, modcase,
*             begingraphics, begindevice, beginframe, getratio,
*             setvport, setworld, endframe, enddevice, endgraphics
*=====================================================================
* MODIFICATION HISTORY:
*    970314:  Allows aspect ratio to float with the frame, or to be
*             fixed to 3/4.  maf
*    900705:  Slightly modified logic to get next SGF or to quit.
*    870930:  Cleaned up considerably.
*    870115:  Original version.
*=====================================================================
* DOCUMENTED/REVIEWED:  870930
*=====================================================================

      include 'mach'

      character message*(mcmsg), prompt*(mcmsg)
      character kfile*(mcpfn), device*(16)
      logical interactive, inputfile
      integer bufAdd	!Added to adjust jbuf for aspect ratio. maf 970314

      parameter (mhelp=4)
      character*(80) khelp(mhelp)
      data khelp/
     # 'This program plots SGFs to another SAC graphics device.',
     # 'First enter the name of the SGF file and the device.',
     # 'After this file has been plotted, enter the name of the',
     # 'next SGF to plot or a blank line to end the program.'/    

* PROCEDURE:

* - Get message from input line or prompt user.

      call zgimsg(message)
      nc=indexb(message)
      if(nc.le.0)then
        do 200 j=1,mhelp
        nc=indexb(khelp(j))
  200   write(munout,'(a)')khelp(j)(1:nc)
        write(munout,'(1x)')
c
c	Modified prompt for aspect ratio.  maf 970314
  10    format("Enter name of SGF, graphics device, and aspect code",
     1         a1," (c constrains the aspect ratio to 3/4): $")
	write(unit=prompt,fmt=10) char(10)
        call zgpmsg(prompt,message)
        interactive=.true.
      else
        interactive=.false.
      endif

* - Decode input.
*   This is the name of an SGF and the graphics device.

      ic=0
      nc=indexb(message)
      call poptok(message,nc,ic,ic1,ic2,itype)
      kfile=message(ic1:ic2)

      call poptok(message,nc,ic,ic1,ic2,itype)
      if(itype.eq.1)then
        device=' '
        call modcase(.true.,message(ic1:ic2),ic2-ic1+1,device)
      else
        device='XWINDOWS'
      endif

* - Determine if aspect ratio floats with the frame, or is constrained to 3/4
*	maf 970314
      call poptok(message,nc,ic,ic1,ic2,itype)
      if(message(ic1:ic1).eq.'c' .or. message(ic1:ic1).eq.'C')then
	fixed=1
	bufAdd=0 
      else
	fixed=0
	bufAdd=5
      endif

* - Initialize graphics library.

      call begingraphics(nerr)
      if(nerr.ne.0)go to 9000
      call begindevice(device,nerr)
      if(nerr.ne.0)go to 9000

* - Art Snokes aspect ratio stuff.  maf 970314
      if(fixed.eq.1)then
	ratio = 0.75
	call setvspacetype(.false.,ratio)
      endif

* - Process each input SGF.

      inputfile=.true.
 1000 if(inputfile)then

* -- Begin frame.
        call beginframe(nerr)
        if(nerr.ne.0)go to 9000

* -- Set up viewport and world coordinates.
      if(fixed.eq.1)then
	call getvspace(xvsmin,xvsmax,yvsmin,yvsmax)	!Aspect ratio fixed to
	call setvport(xvsmin,xvsmax,yvsmin,yvsmax)	!3/4.  maf 970314
      else
        call getratio(ratio)				!Aspect ratio floats
        call setvport(0.0,1.0,0.0,ratio)		!freely
      endif

        call setworld(0.,32000.,0.,24000.)

* -- Plot this file.
        call plotfile(kfile,bufAdd,nerr)	!added bufAdd. maf 970314
        if(nerr.ne.0)go to 8888
        
* -- End frame.
        call endframe(nerr)
        if(nerr.ne.0)go to 9000

* -- Get name of next SGF if appropriate.
        if(interactive .or.
     #     device(1:2).eq.'SU' .or. 
     #     device(1:1).eq.'X')then
          prompt='Enter next SGF or a blank line to end: $'
          call zgpmsg(prompt,message)
          ic=0
          nc=indexb(message)
          call poptok(message,nc,ic,ic1,ic2,itype)
          if(itype.eq.1)then
            kfile=message(ic1:ic2)
          else
            inputfile=.false.
          endif
        else
          inputfile=.false.
        endif

* -- Loop until there are no more input SGFs to process.
        go to 1000
      endif

* - Terminate graphics device and library.

      call enddevice(device,nerr)
      if(nerr.ne.0)go to 9000
      call endgraphics(nerr)
      if(nerr.ne.0)go to 9000

* - Terminate program.

 8888 call zexit

* - Handle exceptions.

 9000 write(munout,'(a,i4)')'ERROR: graphics library error ',nerr
      go to 8888

      end

      subroutine plotfile(kfile,bufAdd,nerr)
*=====================================================================
* PURPOSE:  To plot a SGF to another graphics device.
*=====================================================================
* INPUT ARGUMENTS:
*    kfile:   name of SGF to plot. [c]
*   bufAdd:   added to jbuf to correct for aspect ratio.  maf
*=====================================================================
* OUTPUT ARGUMENTS:
*    nerr:    error return flag. [i]
*=====================================================================
* MODULE/LEVEL:  sgf/2
*=====================================================================
* GLOBAL INPUT:
*    mach:    munout
*=====================================================================
* SUBROUTINES CALLED:
*    local:   plotbuffer
*    saclib:  zopen, zrabs, zclose
*=====================================================================
* MODIFICATION HISTORY:	
*    970314:  Added bufAdd so aspect ratio can be fixed to 3/4. maf
*    870115:  Original version.
*=====================================================================
* DOCUMENTED/REVIEWED:  870115
*=====================================================================

      include 'mach'
      character*(*) kfile
      parameter (mbuf=5200)
      integer*2 buf(mbuf)

      nerr=0

* - Open input SGF.

C      call zgtfun(nun,nerr)
      call zopen(nun,kfile,'RODATA',nlen,nerr)
      if(nerr.ne.0)go to 9000

* - Loop through input SGF, one buffer at a time.

      ilcdsk=0
      ibuf=0
      ldone=.false.
 1000 if(.not.ldone)then

* -- Read in number of long (32-bit) words in next buffer.
        call zrabs(nun,ilnbuf,1,ilcdsk,nerr)
        if (nerr.ne.0) go to 9100
        ilcdsk=ilcdsk+1

* -- Read in buffer.
        call zrabs(nun,buf,ilnbuf,ilcdsk,nerr)
        if (nerr.ne.0) go to 9100
        ilcdsk=ilcdsk+ilnbuf
        ibuf=ibuf+1

* -- Plot contents of buffer.
        nlnbuf=2*ilnbuf
        call plotbuffer(buf,nlnbuf,ldone,bufAdd)    !added bufAdd. maf 970314

* -- Loop until done.
        go to 1000
      endif

* - Close input SGF.

      call zclose(nun,ncerr)

 8888 return

 9000 nc=indexb(kfile)
      write(munout,'(2a)')'ERROR: opening file ',kfile(1:nc)
      go to 8888

 9100 nc=indexb(kfile)
      write(munout,'(2a)')'ERROR: reading file ',kfile(1:nc)
      call zclose(nun,ncerr)
      go to 8888

      end

      subroutine plotbuffer(buf,nlnbuf,ldone,bufAdd)
*=====================================================================
* PURPOSE:  To plot a SGF buffer.
*=====================================================================
* INPUT ARGUMENTS:
*    buf:     buffer of graphics commands. [si]
*    nlnbuf:  (short integer) length of buf. [i]
*    bufAdd:  added to jbuf to correct for aspect ratio. [i] maf
*=====================================================================
* OUTPUT ARGUMENTS:
*    ldone:   set to .true. when end-of-frame command encounted. [l]
*=====================================================================
* MODULE/LEVEL:  sgf/3
*=====================================================================
* GLOBAL INPUT:
*    mach:    mcmsg
*=====================================================================
* SUBROUTINES CALLED:
*    saclib:  worlddraw, worldmove, setcolor, zgetc, text, 
*             settextsize, setlinestyle
*=====================================================================
* MODIFICATION HISTORY:
*    970314:  Added bufAdd to allow fixing the aspect ratio to 3/4. maf
*    870115:  Original version.
*=====================================================================
* DOCUMENTED/REVIEWED:  870115
*=====================================================================

      include 'mach'
      integer*2 buf
      dimension buf(*)
      character*(mcmsg) ktext
      logical firsttime
      data firsttime/.true./

* PROCEDURE:

* - Initialize some variables.

      jbuf=1
      ldone=.false.
      nbadloc=0

* - Loop on each command in buffer:

 1000 if(jbuf.le.nlnbuf)then
        icom=buf(jbuf)

* -- Draw command.
*     "xloc" is real location in the range 0.0 to 32000.
*     "yloc" is real location in the range 0.0 to 24000.

        if(icom.ge.0)then
          call validateloc1(buf(jbuf),buf(jbuf+1),xloc,yloc,nbadloc)
          call worlddraw(xloc,yloc)
          jbuf=jbuf+2

* -- Move command

        elseif(icom.eq.-3)then
          call validateloc1(buf(jbuf+2),buf(jbuf+3),xloc,yloc,nbadloc)
          call worldmove(xloc,yloc)
          jbuf=jbuf+2+buf(jbuf+1)

* -- Color command.
*     "icolor" is a positive integer from 1 to the maximum number
*     of colors in the color table.

        elseif(icom.eq.-4)then
          icolor=buf(jbuf+2)
          call setcolor(icolor)
          jbuf=jbuf+2+buf(jbuf+1)

* -- Hardware text command.
*     "text" is the character string.
*     "nctext" is the character count.

c	    bufAdd added so aspect ratio could be fixed or free. maf 970314
        elseif(icom.eq.-5)then
          nctext=buf(jbuf+2+bufAdd)
          call zgetc(buf(jbuf+3+bufAdd),ktext,nctext)
          if(firsttime)then
            call settexttype('HARDWARE')
            firsttime=.false.
          endif
          call text(ktext,nctext)
          jbuf=jbuf+7+buf(jb2+bufAddf+1)

* -- Hardware text size command.
*     "width" is text width in viewspace coordinates (0.0 to 1.0).
*     "height" is text height.

        elseif(icom.eq.-6)then
          width=float(buf(jbuf+2))/32000.
          height=float(buf(jbuf+3))/32000.
          call settextsize(width,height)
          jbuf=jbuf+2+buf(jbuf+1)

* -- Linestyle command.
*     "iline" is positive integer.
*     A value of 1 is a solid line.  Other values are device dependent.

        elseif(icom.eq.-7)then
          iline=buf(jbuf+2)
          call setlinestyle(iline)
          jbuf=jbuf+2+buf(jbuf+1)

* -- Plot size command.
*     "xsize" is requested horizontal size of plot in inches.
*    (This command is currently ignored in this program.)

        elseif(icom.eq.-8)then
          xsize=float(buf(jbuf+2))/1000.
          jbuf=jbuf+2+buf(jbuf+1)

* -- No-op command.  Used to fill a buffer to a 32-bit word boundary.

        elseif(icom.eq.-1)then
          jbuf=jbuf+1

* -- All done command.

        elseif(icom.eq.-2)then
          ldone=.true.
          jbuf=jbuf+2+buf(jbuf+1)
          go to 8888

* -- Skip unknown opcodes and continue.

        else
          jbuf=jbuf+2+buf(jbuf+1)

        endif

* - Loop until buffer is empty.
        go to 1000

      endif

* - Report any bad locations.

      if(nbadloc.gt.0)then
        write(munout,'(a,i4)')'Bad locations found in file = ',nbadloc
      endif

 8888 return

      end

      subroutine validateloc1(ixin,iyin,xout,yout,nbadloc)
*=====================================================================
* PURPOSE:  To validate a SAC graphics file location.
*           Also converts location from short integers to reals.
*=====================================================================
* INPUT ARGUMENTS:
*    ixin:    sgf x location from file. [si]
*    iyin:    sgf y location from file. [si]
*=====================================================================
* OUTPUT ARGUMENTS:
*    xout:    sgf x location after validation. [f]
*    yout:    sgf y location after validation. [f]
*    nbadloc: incremented by 1 if the input location was bad. [i]
*=====================================================================
* MODULE/LEVEL:  sgf/4
*=====================================================================
* GLOBAL INPUT:
*    mach:
*=====================================================================
* MODIFICATION HISTORY:
*    870929:  Made maximum x and y locations into parameters.
*    851017:  Original version.
*=====================================================================
* DOCUMENTED/REVIEWED:  851017
*=====================================================================

      include 'mach'
      parameter (xmax=32000., ymax=24000.)

      integer*2 ixin, iyin
      real*4 xout, yout

* PROCEDURE:

      xout=ixin
      if(xout .lt. 0.0)then
        xout=0.0
        nbadloc=nbadloc+1
      elseif(xout .gt. xmax)then
        xout=xmax
        nbadloc=nbadloc+1
      endif

      yout=iyin
      if(yout .lt. 0.0)then
        yout=0.0
        nbadloc=nbadloc+1
      elseif(yout .gt. ymax)then
        yout=ymax
        nbadloc=nbadloc+1
      endif

 8888 return

      end
