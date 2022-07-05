      program sgflist
*=====================================================================
* PURPOSE:  To list the contents of a SAC Graphics File (SGF) 
*           in an easily readable alphanumeric format.
*=====================================================================
* MODULE/LEVEL:  sgf/1
*=====================================================================
* GLOBAL INPUT:
*    mach:    mcmsg, mcpfn, mcpw, munout
*=====================================================================
* SUBROUTINES CALLED:
*    local:   listfile
*    saclib:  zgimsg, zgpmsg, indexb, poptok, modcase,
*=====================================================================
* MODIFICATION HISTORY:
*    900312:  Changed scaling factor for plotsize opcode.
*    870930:  Changed name from SGFDUMP.
*             Added output of buffer sizes and number of moves/draws.
*    870112:  Cleaned up and added PLOTSIZE command.
*    850402:  Changes due to modified SGF format and for portability.
*    840629:  Original version.
*=====================================================================
* DOCUMENTED/REVIEWED:  870930
*=====================================================================

      include 'mach'

      character*(mcmsg) message, prompt
      character*(mcpw) token
      character*(mcpfn) kifile, kofile

      parameter (mhelp=7)
      character*(80) khelp(mhelp)
      data khelp/
     # 'This program lists the contents of a SGF in an easily.',
     # 'readable alphanumeric format.  To use enter the name',
     # 'of the SGF, the name of the output file, and any options.',
     # 'Omitting the output name redirects the output to the terminal.',
     # 'Normally the listing includes the number of moves and draws,',
     # 'but does not detail each move and draw.  The option FULL',
     # 'produces a listing which includes each move and draw.'/

* PROCEDURE:

* - Get message from input line or prompt user.

      call zgimsg(message)
      nc=indexb(message)
  100 if(nc.le.0)then
        do 200 j=1,mhelp
        nc=indexb(khelp(j))
  200   write(munout,'(a)')khelp(j)(1:nc)
        write(munout,'(1x)')
        prompt='Enter name of SGF, name of output file, and options: $'
        call zgpmsg(prompt,message)
      endif

* - Decode input.
*   This is the name of the input SGF, the output file, and options.

      lterm=.true.
      lfull=.false.

      ic=0
      nc=indexb(message)
      call poptok(message,nc,ic,ic1,ic2,itype)
      kifile=message(ic1:ic2)

      call poptok(message,nc,ic,ic1,ic2,itype)
      if(itype.gt.0)then
        call modcase(.true.,message(ic1:ic2),ic2-ic1+1,token)
        if(token(1:4).eq.'FULL')then
          lfull=.true.
          lterm=.true.
        else
          lterm=.false.
          kofile=message(ic1:ic2)
        endif
      endif

      call poptok(message,nc,ic,ic1,ic2,itype)
      if(itype.gt.0)then
        call modcase(.true.,message(ic1:ic2),ic2-ic1+1,token)
        if(token(1:4).eq.'FULL')then
          lfull=.true.
        else
          write(munout,'(2a/1x)')'ERROR: unknown option = ',token
          nc=0
          go to 100
        endif
      endif

* - Open output file if appropriate.

      if(lterm)then
        nofun=munout
      else
C        call zgtfun(nofun,nerr)
        if(nerr.ne.0)go to 9000
        call znfile(nofun,kofile,'TEXT',ntused,nerr)
        if(nerr.ne.0)go to 9000
      endif

* - Process input SGF.

      call listfile(kifile,lfull,nofun,nerr)
      if(nerr.ne.0)go to 8888
        
* - Close output file and terminate program.

 8888 if(.not.lterm)call zclose(nofun,ncerr)
      call zexit

* - Handle exceptions.

 9000 nc=indexb(kofile)
      write(munout,'(2a)')'ERROR: opening output file ',kofile(1:nc)
      go to 8888

      end

      subroutine listfile(kifile,lfull,nofun,nerr)
*=====================================================================
* PURPOSE:  To list the contents of a SGF.
*=====================================================================
* INPUT ARGUMENTS:
*    kifile:  name of input SGF to merge. [c]
*    lfull:   .true. if full listing including moves and draws. [l]
*    nofun:   fortran file unit to write to. [i]
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
*    local:   listbuffer
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

* -- List the contents of the buffer.
        nlnbuf=2*ilnbuf
        call listbuffer(buf,nlnbuf,lfull,nofun,ldone)

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

      end

      subroutine listbuffer(buf,nlnbuf,lfull,nofun,ldone)
*=====================================================================
* PURPOSE:  To list a SGF buffer.
*=====================================================================
* INPUT ARGUMENTS:
*    buf:     buffer of graphics commands. [si]
*    nlnbuf:  (short integer) length of buf. [i]
*    lfull:   .true. if full listing, including moves and draws. [l]
*    nofun:   output fortran file unit. [i]
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
      character*(16) fmt1, fmt2, fmt3
      data fmt1/'(a10,2i6)'/
      data fmt2/'(a10,i6,2x,a)'/
      data fmt3/'(a10,2f12.3)'/

* PROCEDURE:

* - Initialize some variables.

      jbuf=1
      ldone=.false.
      nmoves=0
      ndraws=0

* - Write length of buffer:

      write(nofun,fmt1)'bufsize:',nlnbuf

* - Loop on each command in buffer:

 1000 if(jbuf.le.nlnbuf)then
        icom=buf(jbuf)

* -- Draw command.

        if(icom.ge.0)then
          if(lfull)then
            write(nofun,fmt1)'draw:',buf(jbuf),buf(jbuf+1)
          else
            ndraws=ndraws+1
          endif
          jbuf=jbuf+2

* -- Move command

        elseif(icom.eq.-3)then
          if(lfull)then
            write(nofun,fmt1)'move:',buf(jbuf+2),buf(jbuf+3)
          else
            nmoves=nmoves+1
          endif
          jbuf=jbuf+2+buf(jbuf+1)

* -- Color command.

        elseif(icom.eq.-4)then
          if(nmoves.gt.0)then
            write(nofun,fmt1)'nmoves:',nmoves
            nmoves=0
          endif
          if(ndraws.gt.0)then
            write(nofun,fmt1)'ndraws:',ndraws
            ndraws=0
          endif
          write(nofun,fmt1)'color:',buf(jbuf+2)
          jbuf=jbuf+2+buf(jbuf+1)

* -- Hardware text command.

        elseif(icom.eq.-5)then
          if(nmoves.gt.0)then
            write(nofun,fmt1)'nmoves:',nmoves
            nmoves=0
          endif
          if(ndraws.gt.0)then
            write(nofun,fmt1)'ndraws:',ndraws
            ndraws=0
          endif
          nctext=buf(jbuf+2)
          call zgetc(buf(jbuf+3),ktext,nctext)
          write(nofun,fmt2)'hwtext:',nctext,ktext(1:nctext)
          jbuf=jbuf+2+buf(jbuf+1)

* -- Hardware text size command.

        elseif(icom.eq.-6)then
          if(nmoves.gt.0)then
            write(nofun,fmt1)'nmoves:',nmoves
            nmoves=0
          endif
          if(ndraws.gt.0)then
            write(nofun,fmt1)'ndraws:',ndraws
            ndraws=0
          endif
          width=float(buf(jbuf+2))/32000.
          height=float(buf(jbuf+3))/32000.
          write(nofun,fmt3)'hwsize:',width,height
          jbuf=jbuf+2+buf(jbuf+1)

* -- Linestyle command.

        elseif(icom.eq.-7)then
          if(nmoves.gt.0)then
            write(nofun,fmt1)'nmoves:',nmoves
            nmoves=0
          endif
          if(ndraws.gt.0)then
            write(nofun,fmt1)'ndraws:',ndraws
            ndraws=0
          endif
          write(nofun,fmt1)'line:',buf(jbuf+2)
          jbuf=jbuf+2+buf(jbuf+1)

* -- Plot size command.

        elseif(icom.eq.-8)then
          if(nmoves.gt.0)then
            write(nofun,fmt1)'nmoves:',nmoves
            nmoves=0
          endif
          if(ndraws.gt.0)then
            write(nofun,fmt1)'ndraws:',ndraws
            ndraws=0
          endif
          psize=0.01*float(buf(jbuf+2))
          write(nofun,fmt3)'plotsize:',psize
          jbuf=jbuf+2+buf(jbuf+1)

* -- No-op command.  Used to fill a buffer to a 32-bit word boundary.

        elseif(icom.eq.-1)then
          if(nmoves.gt.0)then
            write(nofun,fmt1)'nmoves:',nmoves
            nmoves=0
          endif
          if(ndraws.gt.0)then
            write(nofun,fmt1)'ndraws:',ndraws
            ndraws=0
          endif
          write(nofun,fmt1)'no-op:'
          jbuf=jbuf+1

* -- End-of-plot command.

        elseif(icom.eq.-2)then
          if(nmoves.gt.0)then
            write(nofun,fmt1)'nmoves:',nmoves
            nmoves=0
          endif
          if(ndraws.gt.0)then
            write(nofun,fmt1)'ndraws:',ndraws
            ndraws=0
          endif
          write(nofun,fmt1)'endplot:'
          ldone=.true.
          jbuf=jbuf+2
          go to 8888

* -- Skip unknown opcodes and continue.

        else
          if(nmoves.gt.0)then
            write(nofun,fmt1)'nmoves:',nmoves
            nmoves=0
          endif
          if(ndraws.gt.0)then
            write(nofun,fmt1)'ndraws:',ndraws
            ndraws=0
          endif
          write(nofun,fmt1)'unknown:',icom
          jbuf=jbuf+2+buf(jbuf+1)

        endif

* - Loop until buffer is empty.
        go to 1000

      endif

      if(nmoves.gt.0)then
        write(nofun,fmt1)'nmoves:',nmoves
        nmoves=0
      endif
      if(ndraws.gt.0)then
        write(nofun,fmt1)'ndraws:',ndraws
        ndraws=0
      endif

 8888 return

      end
