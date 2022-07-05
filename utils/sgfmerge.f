      program sgfmerge
*=====================================================================
* PURPOSE:  To merge several SAC Graphics Files (SGF) to a single SGF.
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
*    870930:  Cleaned up considerably.
*    870113:  Minor cleanup.
*    850517:  Original version.
*=====================================================================
* DOCUMENTED/REVIEWED:  870930
*=====================================================================

      include 'mach'

      character*(mcmsg) message, prompt, error
      character*(mcpfn) kifile, kofile
      character*(16) kdevice
      integer*2 buf(2)

      parameter (mhelp=8)
      character*(80) khelp(mhelp)
      data khelp/
     # 'This program merges several SGFs into a single SGF.',
     # 'First enter the name of the output SGF.',
     # 'Then enter the names of each input SGF followed by four',
     # 'space delimted numbers which specify where to place',
     # 'this file in the output SGF.',
     # 'The four numbers are: xmin, xmax, ymin, and ymax.',
     # 'The lower left hand corner is (0.0,0.0). The upper right is',
     # '(1.0,1.0).  Enter a blank line to end the program.'/

* PROCEDURE:

* - Get message from input line or prompt user.

      call zgimsg(message)
      nc=indexb(message)
      if(nc.le.0)then
        do 200 j=1,mhelp
        nc=indexb(khelp(j))
  200   write(munout,'(a)')khelp(j)(1:nc)
        write(munout,'(1x)')
        prompt='Enter name of output SGF: $'
        call zgpmsg(prompt,message)
      endif

* - Decode input.
*   This is the name of the output SGF.

      ic=0
      nc=indexb(message)
      call poptok(message,nc,ic,ic1,ic2,itype)
      kofile=message(ic1:ic2)

* - Open output SGF.

C      call zgtfun(nofun,nerr)
      if(nerr.ne.0)go to 9000
      call znfile(nofun,kofile,'DATA',ntused,nerr)
      if(nerr.ne.0)go to 9000
      ilcout=0

* - Send next prompt and get message back from user.

 1000 prompt='Enter input SGF and plot limits or blank line to end: $'
      call zgpmsg(prompt,message)
      nc=indexb(message)

* - Process each input SGF.

      if(nc.gt.0)then

* -- Decode input.
*    This is the name of the input SGF and plot limits.
        call validateinput(message,kifile,xmin,xmax,ymin,ymax,error)

* -- If error return message is non-blank, print error,get new input and loop.
        nc=indexb(error)
        if(nc.gt.0)then
          write(munout,'(a/1x)')error(1:nc)
          go to 1000
        endif

* -- Scale and merge this file.
        call mergefile(kifile,xmin,xmax,ymin,ymax,nofun,ilcout,nerr)
        if(nerr.ne.0)go to 8888
        
* -- Loop until there are no more input SGFs to process.
        go to 1000
      endif

* - After all input SGF files have been merged, write a new
*   end-of-frame command to output SGF.

      ilnbuf=1
      call zwabs(nofun,ilnbuf,1,ilcout,nerr)
      ilcout=ilcout+1
      buf(1)=-2
      buf(2)=0
      call zwabs(nofun,buf,1,ilcout,nerr)

* - Close output SGF and terminate program.

 8888 call zclose(nofun,ncerr)
      call zexit

* - Handle exceptions.

 9000 nc=indexb(kofile)
      write(munout,'(2a)')'ERROR: opening output file ',kofile(1:nc)
      go to 8888

      end

      subroutine mergefile(kifile,xmin,xmax,ymin,ymax,nofun,ilcout,nerr)
*=====================================================================
* PURPOSE:  To merge a SGF to another SGF.
*=====================================================================
* INPUT ARGUMENTS:
*    kifile:  name of input SGF to merge. [c]
*    xmin:    minimum x location. [f]
*    xmax:    maximum x location. [f]
*    ymin:    minimum y location. [f]
*    ymax:    maximum y location. [f]
*    nofun:   fortran file unit of output SGF. [i]
*    ilcout:  pointer to current output SGF disk location. [i]
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
*    local:   mergebuffer
*    saclib:  zopen, zrabs, zclose
*=====================================================================
* MODIFICATION HISTORY:
*    870115:  Original version.
*=====================================================================
* DOCUMENTED/REVIEWED:  870115
*=====================================================================

      include 'mach'
      character*(*) kifile
      parameter (mbuf=5200)
      integer*2 buf(mbuf)

      nerr=0

* - Calculate scaling constants for this file.

      ixmin=(int(32000.*xmin))
      ixmax=(int(32000.*xmax))
      xscale=float(ixmax-ixmin)/32000.

      iymin=(int(24000.*ymin))
      iymax=(int(24000.*ymax))
      yscale=float(iymax-iymin)/24000.

* - Open input SGF.

C      call zgtfun(nifun,nerr)
      call zopen(nifun,kifile,'RODATA',nlen,nerr)
      if(nerr.ne.0)go to 9000

* - Loop through input SGF, one buffer at a time.

      ilcdsk=0
      ibuf=0
      ldone=.false.
 1000 if(.not.ldone)then

* -- Read in number of long (32-bit) words in next buffer.
        call zrabs(nifun,ilnbuf,1,ilcdsk,nerr)
        if (nerr.ne.0) go to 9100
        ilcdsk=ilcdsk+1

* -- Read in buffer.
        call zrabs(nifun,buf,ilnbuf,ilcdsk,nerr)
        if (nerr.ne.0) go to 9100
        ilcdsk=ilcdsk+ilnbuf
        ibuf=ibuf+1

* -- Scale contents of buffer.
        nlnbuf=2*ilnbuf
        call scalebuffer(buf,nlnbuf,ixmin,iymin,xscale,yscale,ldone)

* -- Write scaled buffer to output SGF.
        call zwabs(nofun,ilnbuf,1,ilcout,nerr)
        if(nerr.ne.0)go to 9200
        ilcout=ilcout+1
        call zwabs(nofun,buf,ilnbuf,ilcout,nerr)
        if(nerr.ne.0)go to 9200
        ilcout=ilcout+ilnbuf
             
* -- Loop until done.
        go to 1000
      endif

* - Close input SGF.

      call zclose(nifun,ncerr)

 8888 return

 9000 nc=indexb(kifile)
      write(munout,'(2a)')'ERROR: opening file ',kifile(1:nc)
      go to 8888

 9100 nc=indexb(kifile)
      write(munout,'(2a)')'ERROR: reading file ',kifile(1:nc)
      call zclose(nifun,ncerr)
      go to 8888

 9200 write(munout,'(a)')'ERROR: writing output file'
      call zclose(nifun,ncerr)
      go to 8888

      end

      subroutine scalebuffer(buf,nlnbuf,ixmin,iymin,xscale,yscale,ldone)
*=====================================================================
* PURPOSE:  To scale a SGF buffer.
*=====================================================================
* INPUT ARGUMENTS:
*    buf:     buffer of graphics commands. [si]
*    nlnbuf:  (short integer) length of buf. [i]
*    ixmin:   starting x location in SGF units. [i]
*    iymin:   starting y location in SGF units. [i]
*    xscale:  x scaling factor. [f]
*    yscale:  y scaling factor. [f]
*=====================================================================
* OUTPUT ARGUMENTS:
*    ldone:   set to .true. when end-of-frame command encounted. [l]
*=====================================================================
* MODULE/LEVEL:  sgf/3
*=====================================================================
* GLOBAL INPUT:
*    mach:    mcmsg
*=====================================================================
* MODIFICATION HISTORY:
*    870115:  Original version.
*=====================================================================
* DOCUMENTED/REVIEWED:  870115
*=====================================================================

      include 'mach'
      integer*2 buf
      dimension buf(*)
      character*(mcmsg) ktext

* PROCEDURE:

* - Initialize some variables.

      jbuf=1
      ldone=.false.
      nbadloc=0

* - Loop on each command in buffer:

 1000 if(jbuf.le.nlnbuf)then
        icom=buf(jbuf)

* -- Draw command.

        if(icom.ge.0)then
          buf(jbuf  )=ixmin+int(float(buf(jbuf  ))*xscale)
          buf(jbuf+1)=iymin+int(float(buf(jbuf+1))*yscale)
          jbuf=jbuf+2

* -- Move command

        elseif(icom.eq.-3)then
          buf(jbuf+2)=ixmin+int(float(buf(jbuf+2))*xscale)
          buf(jbuf+3)=iymin+int(float(buf(jbuf+3))*yscale)
          jbuf=jbuf+2+buf(jbuf+1)

* -- Color command.

        elseif(icom.eq.-4)then
          jbuf=jbuf+2+buf(jbuf+1)

* -- Hardware text command.

        elseif(icom.eq.-5)then
          jbuf=jbuf+2+buf(jbuf+1)

* -- Hardware text size command.

        elseif(icom.eq.-6)then
          buf(jbuf+2)=ixmin+int(float(buf(jbuf+2))*xscale)
          buf(jbuf+3)=iymin+int(float(buf(jbuf+3))*yscale)
          jbuf=jbuf+2+buf(jbuf+1)

* -- Linestyle command.

        elseif(icom.eq.-7)then
          jbuf=jbuf+2+buf(jbuf+1)

* -- Plot size command.

        elseif(icom.eq.-8)then
          jbuf=jbuf+2+buf(jbuf+1)

* -- No-op command.  Used to fill a buffer to a 32-bit word boundary.

        elseif(icom.eq.-1)then
          jbuf=jbuf+1

* -- All done command.
*    Substitute no-ops for these until all files are merged.
        elseif(icom.eq.-2)then
          buf(jbuf  )=-1
          buf(jbuf+1)=-1
          ldone=.true.
          jbuf=jbuf+2
          go to 8888

* -- Skip unknown opcodes and continue.

        else
          jbuf=jbuf+2+buf(jbuf+1)

        endif

* - Loop until buffer is empty.
        go to 1000

      endif

 8888 return

      end

      subroutine validateinput(message,kifile,xmin,xmax,ymin,ymax,error)
*=====================================================================
* PURPOSE:  To validate an input message in sgfmerge.
*=====================================================================
* MODULE/LEVEL:  sgf/4
*=====================================================================
* GLOBAL INPUT:
*    mach:    mcmsg, munout
*=====================================================================
* SUBROUTINES CALLED:
*    saclib:  indexb, poptok
*=====================================================================
* MODIFICATION HISTORY:
*    870930:  Original version.
*=====================================================================
* DOCUMENTED/REVIEWED:  870930
*=====================================================================

      include 'mach' 
      character*(*) message, kifile, error
      
* - Initialize return error message.

      error=' '

* - Get filename.

      ic=0
      nc=indexb(message)
      call poptok(message,nc,ic,ic1,ic2,itype)
      kifile=message(ic1:ic2)

* - Loop on rest of tokens in message.

      nvalue=0
 1000 call poptok(message,nc,ic,ic1,ic2,itype)

* -- If a token exists.
      if(itype.gt.0)then
* --- Increment counter and convert to floating point.
        nvalue=nvalue+1
        call cnvatf(message(ic1:ic2),value,nerr)
* --- Error if can't convert.
        if(nerr.ne.0)then
          error='At least one of the plot limits is not a number.'
          go to 8888
* --- Range check minimum x value.
        elseif(nvalue.eq.1)then
          if(value .ge. 0.0 .and. value .le. 1.0)then
            xmin=value
          else
            error='Value for xmin is not in the proper range.'
            go to 8888
          endif
* --- Range check maximum x value.
        elseif(nvalue.eq.2)then
          if(value .ge. xmin .and. value .le. 1.0)then
            xmax=value
          else
            error='Value for xmax is not in the proper range.'
            go to 8888
          endif
* --- Range check minimum y value.
        elseif(nvalue.eq.3)then
          if(value .ge. 0.0 .and. value .le. 1.0)then
            ymin=value
          else
            error='Value for ymin is not in the proper range.'
            go to 8888
          endif
* --- Range check maximum y value.
        elseif(nvalue.eq.4)then
          if(value .ge. ymin .and. value .le. 1.0)then
            ymax=value
          else
            error='Value for ymax is not in the proper range.'
            go to 8888
          endif
* --- Error if too many values.
        else
          error='More than four values entered for plot limits.'
          go to 8888
        endif

* -- Loop until there are no more values
        go to 1000

* -- Error if too few values.
      elseif(nvalue.lt.4)then
        error='Less than four values entered for plot limits.'
        go to 8888
      endif

 8888 return

      end
