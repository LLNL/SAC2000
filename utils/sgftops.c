/*
*=====================================================================
* PURPOSE:  To convert SAC Graphics Files to a Postscript File.
*=====================================================================
* MODULE/LEVEL:  sgfcodes/1
*=====================================================================
* DEPENDENT ROUTINES:
*   sgftoeps:  Produces encapsulated PS for FrameMaker.
*=====================================================================
* MODIFICATION HISTORY:
* NOTE: Additions and deletions to this program may affect sgftoeps.
*    920812:  Merged filled rectangles and colortables from Stephanie Daveler.
*    920811:  Merged filled polygons from Chuck Ammon and Myers (U of Arizona)
*    920522:  Added line-width opcode (-9) command.
*    910309:  Changed the "%%Page: ? 1" to "%%Page: ? ?" - fixes 
*             duplicate page problem.
*    910128:  Added %%Page: ? 1    enables pageview to preview ps file.
*    900710:  Added flag on command line to ignore scaling.
*    900709:  Added prompt for scaling when scale parameter is 32000.
*    900323:  Increased available linestyles from 5 to 10.
*             Added mod 10 to line style selection.
*    900312:  Updated op-code tests to match fortran version.
*             Added new 10" width scaling factor and plotsize opt.
*    870921:  Added optional linewidth and plot id label plus bug fixes
*    870502:  Made it more robust and more readable
*    870113:  Modified input arguments and some logic.
*    860000:  Original version due to Tom Owens at Univ. of Missouri.
*=====================================================================
*/
#include <stdio.h>
#include <sys/param.h>

#define TRUE 1
#define FALSE 0
#define maxbuflen 5004
#define NUMLSTYLS 10                              /* 10 line styles */
#define NUMLWIDTHS 10                             /* 10 line widths */

/*
  Available Line Styles: 
       styles are defined later by:  "[bu wu bu wu] offset"
       where bu=black units, wu=white units, offset=0.
       Numbers are large because of reduced scaling (.0225) 
       of the sgf file (32000 X 2400) onto 10" X 7.5" plot.

    style  description                      PS setdash definition
    -----  -------------------------------  ---------------------
     ls1:  solid                            [] 0
     ls2:  dots                             [50 75] 0
     ls3:  sm dashes                        [300 200] 0
     ls4:  lg dashes                        [600 300] 0
     ls5:  lg dash, dot                     [600 200 75 200] 0
     ls6:  lg dash, sm dash                 [600 200 300 200] 0
     ls7:  lg dash, dot, dot                [600 200 75 200 75 200] 0
     ls8:  lg dash, sm dash, sm dash        [600 200 300 200 300 200] 0
     ls9:  lg dash, dot, dot, dot           [600 200 75 200 75 200 75 200] 0
     ls10: lg, sm, sm, sm dashes            [600 200 300 200 300 200 300 200] 0
*/

static char *linemodes[] = {
  "ls1","ls2","ls3","ls4","ls5",
  "ls6","ls7","ls8","ls9","ls10"
  };

static char scale[]="yes";            /* for prompt or command line parameter */
static int scale_flag = TRUE;

static int textangle = 0;
static int colortable = FALSE;
static char *colorfile = "colortable";

static int imagecount = 0;
static int icount = 0;
static int doing_image = FALSE;

int ivec=400; /* global count for path length, then stroke */

/*----------------------------------------------------------------------------*/
main(argc,argv)
     int argc;
     char *argv[];
{
    short *buffer, *tempptr;
    int nbuffer;
    int fp, buflen, idx, width, done = 0, ict;
    FILE  *fopen(), *ofp, *fpc;
    float red[256], green[256], blue[256];
    char *p1, *get_nextarg();
    char *dattim, usnam[L_cuserid+2], path[MAXPATHLEN];
    char *ctime();
    int rawtim;

    /* be sure we have enough arguments */
    if (argc < 3 ) {
	fprintf(stderr,"usage: %s sgf_file ps_file",*argv);
	fprintf(stderr," [ line_width id_flag scale_flag]\n");
	fprintf(stderr," where:  line_width = 1, 2, or 3\n");
	fprintf(stderr,"         id_flag    = yes or no\n");
	fprintf(stderr,"         scale_flag = yes or no\n");
	fprintf(stderr,"  example: sgftops foo.sgf foo.ps 2 yes no\n");
	fprintf(stderr,"      for line width 2, id label, no scaling\n");
	exit(-1);
    }

    /* does argv[1] end in ".sgf" ? */
    if ( strcmp(&argv[1][strlen(argv[1])-4],".sgf") ) {
	fprintf(stderr,"%s: %s not a SAC GRAPHICS FILE.\n",*argv,argv[1]);
	exit(1);
    }

    /* open the sgf file */
    fp = open(argv[1],0);
    if (fp == -1) {
	fprintf(stderr,"%s: Can't open %s for reading.\n",*argv,argv[1]);
	exit(-3);
    }

    /* open the postscript file to write */
    if ((ofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"cannot open postscript file to write: '%s'\n",argv[2]);
	exit(-2);
    }

    /* initialize the buffer */
    if((buffer = (short *)malloc(maxbuflen*sizeof(short))) == NULL){
	fprintf(stderr,"cannot create input buffer\n");
	exit(-4);
    }

    nbuffer = maxbuflen*sizeof(short);

    /* get the info for the plot id label */
    rawtim = time(0);
    dattim = ctime(&rawtim);
    cuserid(usnam);
    getcwd(path,(size_t)MAXPATHLEN);

    /* define some abreviations for some postscript commands */
    /* and use optional line width if argument present       */
    fprintf(ofp,"%%!PS-Adobe-1.0\n");
    fprintf(ofp,"/Helvetica findfont 8 scalefont setfont");
    fprintf(ofp,"\n/ls1 {[] 0 setdash} def");
    fprintf(ofp,"\n/ls2 {[50 75] 0 setdash} def");
    fprintf(ofp,"\n/ls3 {[300 200] 0 setdash} def");
    fprintf(ofp,"\n/ls4 {[600 300] 0 setdash} def");
    fprintf(ofp,"\n/ls5 {[600 200 75 200] 0 setdash} def");
    fprintf(ofp,"\n/ls6 {[600 200 300 200] 0 setdash} def");
    fprintf(ofp,"\n/ls7 {[600 200 75 200 75 200] 0 setdash} def");
    fprintf(ofp,"\n/ls8 {[600 200 300 200 300 200] 0 setdash} def");
    fprintf(ofp,"\n/ls9 {[600 200 75 200 75 200 75 200] 0 setdash} def");
    fprintf(ofp,"\n/ls10 {[600 200 300 200 300 200 300 200] 0 setdash} def");
    fprintf(ofp,"\n/L { lineto } def");
    fprintf(ofp,"\n/m { moveto } def");
    fprintf(ofp, "\n/g { setgray } def");
    fprintf(ofp, "\n/s { stroke } def");

    /* define procedure to make filled rectangles */
    fprintf(ofp,"\n/F {");
    fprintf(ofp,"\n   /blue exch def");
    fprintf(ofp,"\n   /green exch def");
    fprintf(ofp,"\n   /red exch def");
    fprintf(ofp,"\n   /dy exch def");
    fprintf(ofp,"\n   /dx exch def");
    fprintf(ofp,"\n   /y exch def");
    fprintf(ofp,"\n   /x exch def"); 
    fprintf(ofp,"\n   stroke");
    fprintf(ofp,"\n   red green blue setrgbcolor");
    fprintf(ofp,"\n   x y m");
    fprintf(ofp,"\n   0 dy rlineto");
    fprintf(ofp,"\n   dx 0 rlineto");
    fprintf(ofp,"\n   0 dy neg  rlineto");
    fprintf(ofp,"\n   closepath");
    fprintf(ofp,"\n   fill");
    fprintf(ofp,"\n   } def");

    idx = 3;
    if ((p1=get_nextarg(argc,argv,&idx)) != NULL) {
	width = *p1 - '0';
	if ( width < 0  ||  width > 9 ) width = 1;
	width *= 15;
	fprintf(ofp,"\n%d setlinewidth",width);
    } 
    else
	fprintf(ofp,"\n%d setlinewidth",15);

/* The next command makes the ps file preview in OpenWindow's pageview. */
    fprintf(ofp,"\n%%%%Page: ? ?"); 

/* 
  WARNING: sgftoeps skips three lines after "gsave" to insert new
            translate and scale commands. Modify sgftoeps if adding
            lines after gsave. (wct)
*/
    fprintf(ofp,"\ngsave");
    fprintf(ofp,"\n72 8 mul 36 translate");
    fprintf(ofp,"\n90 rotate");
    fprintf(ofp,"\n10 72 mul 32000 div 7.5 72 mul 24000 div scale");

    /* add the optional label for userid, dir, file, date and time */
    if ((p1=get_nextarg(argc,argv,&idx)) != NULL) {
	if ( !strncmp(p1,"yes",3) ) {
	    fprintf(ofp,"\n20 20 m");
	    fprintf(ofp,"\ngsave");
	    fprintf(ofp,"\n32000 72 10 mul div 24000 72 7.5 mul div scale");
	    fprintf(ofp,"\n(%s          )show",usnam);
	    fprintf(ofp,"\n(%s          )show",path);
	    fprintf(ofp,"\n(%s          )show",argv[1]);
	    fprintf(ofp,"\n(%200.24s )show",dattim);
	    fprintf(ofp,"\nstroke");
	    fprintf(ofp,"\ngrestore");
	}
    }

    /* set the scale flag on or off */
    if ((p1=get_nextarg(argc,argv,&idx)) != NULL) 
	if ( !strncmp(p1,"no",2) ) scale_flag = FALSE;

    /* Make sure we've gobbled up all arguments */
    get_nextarg(argc,argv,&idx);

    /* look for colortable file */
    ict = 0;
    if (colortable) {
	if ((fpc = fopen(colorfile,"r")) == NULL) {
	    fprintf(stderr,"%s: %s color table file missing.\n",*argv,colorfile);
	    exit(1);
	}
	/* user defined colortable */
	while (fscanf(fpc,"%g %g %g",&red[ict],&green[ict],&blue[ict]) == 3) ict ++;
	fclose(fpc);
    }
    else {
	/* default colortable */
	red[ict] = 1.0; green[ict] = 1.0; blue[ict++] = 1.0;
	red[ict] = 1.0; green[ict] = 0.0; blue[ict++] = 0.0;
	red[ict] = 0.0; green[ict] = 1.0; blue[ict++] = 0.0;
	red[ict] = 0.0; green[ict] = 0.0; blue[ict++] = 1.0;
	red[ict] = 1.0; green[ict] = 1.0; blue[ict++] = 0.0;
	red[ict] = 0.0; green[ict] = 1.0; blue[ict++] = 1.0;
	red[ict] = 1.0; green[ict] = 0.0; blue[ict++] = 1.0;
	red[ict] = 0.0; green[ict] = 0.0; blue[ict++] = 0.0;
    }

    /* execute this loop once for each input record */
    while (!done) {
	if ( read(fp,&buflen,4) != 4 )
	    break;

	buflen *= 2;
    
    /* 
      Buflen is now the number of short integers (not including the 4 bytes
      comprising buflen) in the next record.  
      NOTE: read uses byte count and execute_buffer use a count of short
      integers, hence the repeated *2 and /2 operations, but this
      will take care of any truncated files.
    */

	if( (2*buflen) > nbuffer ){
	    if((tempptr = (short *)realloc(buffer,(2*buflen))) == NULL) {
		fprintf(stderr,"could not expand input buffer-quitting\n");
		exit(-5);
	    }
	    buffer = tempptr;
	    nbuffer = 2*buflen;
	}

	if ( (buflen = read(fp,buffer,2*buflen)) < 0 )
	    break;

	execute_buffer(buffer,buflen/2,&done,ofp,red,green,blue,ict);
    } /* while */

    close(fp);

    /* print the page */
    fprintf(ofp,"\nstroke");
    fprintf(ofp,"\nshowpage");
    fprintf(ofp,"\ngrestore");
    fprintf(ofp,"\n");
    fprintf(ofp,"%%%%EndProlog\n%%%%Trailer\n");
    (void) fclose(ofp);
    exit(0);
} /* end main */

/* -----------------------------------------------------------------------*/
execute_buffer(buffer,buflen,done,ofp,red,green,blue,ict)
short *buffer;
int buflen;
int *done;
FILE *ofp;
float red[], green[], blue[];
{
    int idx,indx; /* ivec is global now, so short buffers are ok */
    int x,y,count,dx,dy;
    int iwidth, iheight, xloc, yloc, bufcount, nwrite, charcount;
    char str[133];
    char ibuf[25];

    float width, height;                     /* width and height of viewspace */
                                             /* for hardware text size */

    float xsize;                             /* requested horizontal size in " */
    float xscale, yscale;
	
    static int fillFlag=0;
    int igray;
    unsigned short hexnum;
    unsigned char *cptr;
    float gray_value;

#ifdef DEBUG  
    printf("buflen = %d\n",buflen);
#endif

    /* are we in the middle of writing out an image? */
    if( doing_image ){
	/* print out rgb values 24 */
	/* to a line.              */

	bufcount = 0;
	charcount = 0;

	cptr = (unsigned char *)buffer;

	while ( ((icount+24) <= imagecount) && 
                ((bufcount + 12) <= buflen) ){
	    for( idx=0; idx<24; idx++){ 
		hexnum = cptr[charcount++];
		fprintf(ofp,"%2.2hx",hexnum);
	    }
	    fprintf(ofp,"\n");
	    icount += 24;
	    bufcount += 12;
	} /* end while */

	if((buflen-bufcount) > 0){
	    nwrite = 2*(buflen-bufcount);
	    nwrite = (icount+nwrite) <= imagecount ? nwrite: imagecount-icount;
	    icount += nwrite;
	    for( idx=0; idx<nwrite; idx++){
		hexnum = cptr[charcount++];
		fprintf(ofp,"%2.2hx",hexnum);
	    }
	    fprintf(ofp,"\n");
	}

	if(icount >= imagecount){
	    doing_image = FALSE;
	    fprintf(ofp,"\n grestore");
	}

	return;
    } /* end if ( doing_image ) */


    for (idx = 0; idx < buflen && !*done; ) {
	ivec--;
	if (ivec == 0) {
	    /*fprintf(ofp,"\nstroke");
	    fprintf(ofp,"\n%d %d m",x,y);*/
	    ivec = 400;
	}
	if (buffer[idx] >= 0) {
	    x = buffer[idx++];
	    y = buffer[idx++];
	    fprintf(ofp,"\n%d %d L",x,y);
	}
	else {
	    switch(buffer[idx++]) {
		case -1 : break;                 /* No-op command */
		case -2 : *done = 1;             /* End of plot command */
		    count = buffer[idx++];
		    break;
		case -3 : count = buffer[idx++];   /* Move command */
		    x = buffer[idx++];
		    y = buffer[idx++];
		    EndPath(ofp,&fillFlag);
		    fprintf(ofp,"\n%d %d m",x,y);
		    break;
		case -4 : count = buffer[idx++];   /* Color command - default SAC color map  */
		    indx = buffer[idx++];
		    if (indx < ict) {
			EndPath(ofp,&fillFlag);
			fprintf(ofp,"\n%g %g %g setrgbcolor",
			    red[indx], green[indx], blue[indx]);
		    }
		    else {
			EndPath(ofp,&fillFlag);
			fprintf(ofp,"\n0.0 0.0 0.0 setrgbcolor");
			printf("Illegal color value encountered - set to default color black\n");
		    }
		    break;                               
		case -5 : count = buffer[idx++];               /* Hardware text command */
		    idx += 5;  /* modified due to change in hardware text spec. by Ammon 10-3-94 */
		    y = buffer[idx++];
		    for (x = 0; x < y; x += 2) {
			str[x] = ((char *)buffer)[2*idx];
			str[x+1] = ((char *)buffer)[2*idx+1];
			idx++;
		    }
		    str[y] = '\0';
		    fprintf(ofp,"\ngsave");
		    if (textangle != 0) fprintf(ofp,"\n%d rotate",textangle);
		    fprintf(ofp,"\n32000 72 10 mul div 24000 72 10 mul div scale");
		    fprintf(ofp,"\n(%s)show",str);     /* Not in F77 version ? */
		    fprintf(ofp,"\ngrestore");
		    break;
		case -6 : count = buffer[idx++];               /* Hardware text size */
		    width = (float)buffer[idx++]/32000.; /* (ignored ) */
		    height =(float)buffer[idx++]/32000.;
		    break;
		case -7 : count = buffer[idx++];               /* Linestyle command */
		    EndPath(ofp,&fillFlag);
		    if (buffer[idx] <=0)
			buffer[idx] = 1;                
		    fprintf(ofp,"\n%s",linemodes[(buffer[idx++]-1)%NUMLSTYLS]);
		    break;
		case -8 : count=buffer[idx++];                 /* Plot size command */
		    if (buffer[idx] == 32000 && scale_flag) {
			printf("Scale size 32000 encountered. \n");
			printf("Possible Old SGF - Scale anyway (y/n):");
			gets(scale);
		    }
	
		    if ( !strncmp(scale,"n",1) || !scale_flag )  /* don't scale */
			idx++;
		    else {
			xsize=(float)buffer[idx++]*0.01;
			yscale=xscale=xsize/10.0;
			fprintf(ofp,"\n%14.3f%14.3f scale", xscale,yscale);
		    }
		    break;
		case -9 : count = buffer[idx++];               /* Line-width command */
		    EndPath(ofp,&fillFlag);
		    buffer[idx] = buffer[idx] <=0 ? 0 : buffer[idx] ;
		    fprintf(ofp,"\n %d setlinewidth", buffer[idx++]*15 );
		    break;

		case -10:				/* Polyfill Command */
		    count = buffer[idx++];
		    igray = buffer[idx++];
		    if (igray == 0)
			gray_value = 1.0;
		    if (igray == 1)
			gray_value = 0.8;
		    if (igray == 2)
			gray_value = 0.0;
		    count = buffer[idx++];
		    count = buffer[idx++];	/* Move command */
		    x = buffer[idx++];
		    y = buffer[idx++];
		    EndPath(ofp,&fillFlag);
		    fprintf(ofp, "\n%.1f g", gray_value);
		    fprintf(ofp, "\n%d %d m", x, y);
		    fillFlag = 1;
		    break;
		case -11: count = buffer[idx++];             /* plot filled rectangle */
		    x = buffer[idx++];
		    y = buffer[idx++];
		    dx = buffer[idx++];
		    dy = buffer[idx++];
		    indx = buffer[idx++];
		    if (indx >= ict) {
			printf("Illegal color value - set to last color index\n");
			indx = ict-1;
		    }
		    fprintf(ofp,"\n%d %d %d %d %g %g %g F",x,y,dx,dy,
			    red[indx], green[indx], blue[indx]);
		    break;
		case -12: count = buffer[idx++];             /* plot filled rectangle */
		    textangle = buffer[idx++];
		    break;
		case -13: iwidth = buffer[idx++];             /* color image */
		    iheight = buffer[idx++];
		    xloc   = buffer[idx++];
		    yloc   = buffer[idx++];
		    fprintf(ofp,"\n gsave");
		    fprintf(ofp,"\n /picstr %d string def", 3*iwidth);
		    fprintf(ofp,"\n %d %d translate",xloc,yloc);
		    fprintf(ofp,"\n %d %d scale",32*iwidth,32*iheight);
		    fprintf(ofp,"\n %d %d 8",iwidth,iheight);
		    fprintf(ofp,"\n [ %d 0 0 %d 0 %d ]",iwidth,-iheight,iheight);
		    fprintf(ofp,"\n {currentfile\n picstr readhexstring pop}");
		    fprintf(ofp,"\n false 3");
		    fprintf(ofp,"\n colorimage\n\n");

		    imagecount = iwidth*iheight*3;
		    doing_image = TRUE;
		    icount = 0;

		    break;        
		default : count = buffer[idx++];               /* Skip unknown op-code */
		    printf("unknown op code: %idx",count);
		    idx += count;
	    } /* end switch */
	} /* end else */
    } /* end for */
} /* end execute_buffer */


/***************************************************************************/

EndPath(ofp, fillFlag)
int    *fillFlag;
FILE   *ofp;
{
	if (! *fillFlag)
		fprintf(ofp, "\ns");
	else if (*fillFlag == 1) {
		fprintf(ofp, "\nfill\n0.0 g");
		*fillFlag = 0;
	}
}

/***************************************************************************/
/*---------------------------------------------------------------------------*/
/* read a short integer from a stream */
short getshort(fp)
FILE *fp;
{
  int hi;
  
  hi =	getc(fp);
  return((hi << 8) + getc(fp));
}

char *get_nextarg(argc,argv,i)
int argc;
char *argv[];
int *i;
{
    char *nextarg;

    nextarg = NULL;
    if (argc > *i) {
	nextarg = argv[*i];
	*i = *i + 1;
	if ((nextarg[0] == '-') && (nextarg[1] == 'c')) {
	    colortable = TRUE;
	    if (strlen(nextarg) > 2)
		colorfile = &nextarg[2];
	    return(get_nextarg(argc,argv,i));
	}
	else
	    return(nextarg);
    }
    else
	return(NULL);
}
/* eof */
