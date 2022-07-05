/*
 * sgftoai                       VERSION:  1.0                   06 March 1994
 * 
 * 
 * Chuck Ammon, ammon@slueas.slu.edu
 * 
 * 
 * PURPOSE:  To convert SAC Graphics Files to an Adobe Illustrator 88 Files
 * 
 * 
 * GENERAL COMMENTS: This is not all my code however, for readibility, I usually
 * name all global variables beginning with a lower case g and define'd
 * variables with a 'k', functions begin with upper case letters. Some of
 * these conventions may be ignored by older parts of the code.
 * 
 * requires the math library (-lm) and the file adobe1.20.h change the path name
 * for adobe1.20.h ( 1 instance ) compile: cc -o sgftoadobe sgftoadobe.c -lm
 * 
 * The units of the plot are converted from sgf units to points, where 1 point =
 * 1/72 inches.
 * 
 */
#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>
#include <math.h>

#define kMaxPtsPerPoly 2750	/* maximum points in any one path */
#define kMaxBufferLength 7500
#define kX_OFFSET 106		/* used in function ToPoints */
#define kY_OFFSET 306

#define kLeft 1			/* used for aligning text */
#define kCenter 2
#define kRight 3
#define kBottom 1
#define kTop 3

char           *gLineThicknesses[5] = {"0.3 w", "1.5 w", "2.5 w", "3.5 w", "4.0 w"};

char           *gLineDashes[5] = {"[]0 d", "[1 3]0 d", "[1 2 4 2]4 d", "[3]0 d", "[4 2]0 d"};

/* white red green blue yellow cyan magenta black */

char           *gColorOp[8] = {"0 0 0 1 K", "0 1 1 0 K", "1 0 1 0 K", "1 1 0 0 K", "0 0 1 0 K", "1 0 0 0 K", "0 1 0 0 K", "1 1 1 1 K"};

int             gcolorFlag = 0, gFillFlag = -1;
int             gFirstTimeThrough = 1;
int             gColorSwitch = 1;
char            gLineType = 't';/* t = thickness (default), d = dashes */

#define kStroke 1
#define kFill 2

int             gStroke_or_Fill = kStroke;
float           gGrayValue = 0.0;

/*******************************************************************

global variables for coordinate conversions

*******************************************************************/

float           gXfactor = 0.0135;	/* 6" by 4.5" view space */
float           gYfactor = 0.0135;
/* gXfactor =  0.0225  for 10" by 7.5" view space */

#define minDist 0.024		/* 1/3000 of an inch = 0.024 points */
/* minDist 0.48   1/150 of an inch = 0.24 points */

float           gOldX = -1000, gOldY = -1000;

int             gToF;
float           gXmax = 0, gXmin = 1000;
float           gYmax = 0, gYmin = 1000;

/* end of coordinate globals */

/*******************************************************************

		MAIN

*******************************************************************/
main(argc, argv)
	int             argc;
	char           *argv[];
{
	short           buffer[kMaxBufferLength];
	int             buflen, i, width, done = 0;
	FILE           *fp, *fopen(), *ofp;
	char           *p1, *p2;
	char            includeShowpage = 'n';
	char            headerType = 'l';
	short           GetShort();
	char           *dattim, usnam[L_cuserid + 2], path[MAXPATHLEN];
	char           *ctime();
	int 		rawtim;

	/* be sure we have enough arguments */
	if (argc < 3 || argc > 6) {
		AboutSGFtoAI();
		exit(-1);
	}
	/* does argv[1] end in ".sgf" ? */
	if (strcmp(&argv[1][strlen(argv[1]) - 4], ".sgf") != 0) {
		fprintf(stderr, "%s: %s not a SAC GRAPHICS FILE.\n", *argv, argv[1]);
		exit(1);
	}
	/* open the sgf file */
	fp = fopen(argv[1], "r");
	if (fp == NULL) {
		fprintf(stderr, "%s: Can't open %s for reading.\n"
			,*argv, argv[1]);
		exit(-3);
	}
	/* open the postscript file to write */
	if ((ofp = fopen(argv[2], "w")) == NULL) {
		fprintf(stderr, "cannot open postscript file to write: '%s'\n", argv[2]);
		exit(-2);
	}
	
	/*******************************************************************
	
		set the linestyle, showpage, and header flags
	*/
	
	if (argc > 3) {
		p2 = argv[3];
		headerType = *p2;
		if (headerType != 'l')
			gColorSwitch = 0;
		if (headerType == 'i')
			includeShowpage = 'y';
	}
	if (argc > 4) {
		p1 = argv[4];
		gLineType = *p1;
	}
	if (argc > 5) {
		p2 = argv[5];
		includeShowpage = *p2;
	}
	/*******************************************************************
	
		Get the file information for EPSF comments
	
	*/
	rawtim = time(0);
	dattim = ctime(&rawtim);
	cuserid(usnam);
	getwd(path);

	fprintf(ofp, "%%!PS-Adobe-2.0 EPSF-1.2\n");
	fprintf(ofp, "%%%%Creator: Adobe Illustrator 88(TM) 1.8.3\n");
	fprintf(ofp, "%%%%For: (%s)\n", usnam);
	fprintf(ofp, "%%%%Title: (%s/%s)\n", path, argv[1]);
	fprintf(ofp, "%%%%CreationDate:  (%.24s)\n", dattim);

	/*******************************************************************
	
		Copy the adobe prolog and set initialize the
		graphics state
	
	*/
	CopyProlog(ofp, headerType);
	Initialize(ofp);


	/*******************************************************************
	
		Loop through the sgf file
	
	*/

	/* execute this loop once for each input record */
	while (!done) {
		buflen = GetShort(fp);
		if (buflen < 0)
			break;
		buflen = (buflen << 16) + GetShort(fp);
		buflen *= 2;
		/*
		 * Buflen is now the number of short integers (not including
		 * the 4 bytes comprising buflen) in the next record.
		 */

		for (i = 0; i < buflen; i++)
			buffer[i] = GetShort(fp);
		HandleBuffer(buffer, buflen, &done, ofp);
	}
	(void) fclose(fp);

	/*******************************************************************
	
		Finish up with the trailing postscript
	
	*/

	if (includeShowpage == 'y')
		fprintf(ofp, "showpage\n");
	fprintf(ofp, "%%%%Trailer\n");

	if (headerType == 'l') {
		fprintf(ofp, "Adobe_Illustrator881 /terminate get exec\n");
		fprintf(ofp, "Adobe_pattern /terminate get exec\n");
		fprintf(ofp, "Adobe_customcolor /terminate get exec\n");
		fprintf(ofp, "Adobe_cshow /terminate get exec\n");
		fprintf(ofp, "Adobe_packedarray /terminate get exec\n");
	}
	(void) fclose(ofp);
	exit(0);
}

/*******************************************************************/

HandleBuffer(buffer, buflen, done, ofp)
	short          *buffer;
	int             buflen;
	int            *done;
	FILE           *ofp;
{
	int             i;
	int             x, y, count, dummy, colorIndex, igray;
	float           fx, fy, a, b, angle, deg_to_rad = 0.017453292;
	char            str[133];
	int             hjust, vjust;
	static short    xp[kMaxPtsPerPoly], yp[kMaxPtsPerPoly];
	static int      fontsize, npts = 0;

	for (i = 0; i < buflen && !*done;) {

		if (npts == kMaxPtsPerPoly) {
			PaintPath(xp, yp, &npts, ofp);
		}
		if (buffer[i] >= 0) {
			xp[npts] = buffer[i++];
			yp[npts] = buffer[i++];
			npts += 1;

		} else
			switch (buffer[i++]) {
			case -1:
				break;
			case -2:
				PaintPath(xp, yp, &npts, ofp);
				*done = 1;
				count = buffer[i++];
				break;
			case -3:
				PaintPath(xp, yp, &npts, ofp);
				count = buffer[i++];	/* MoveTo */
				xp[npts] = buffer[i++];
				yp[npts] = buffer[i++];
				npts += 1;
				break;
			case -4:
				PaintPath(xp, yp, &npts, ofp);

				count = buffer[i++];	/* the color operator */
				colorIndex = buffer[i++];
				fprintf(ofp, "%s\n", gColorOp[colorIndex]);
				break;
			case -5:
				PaintPath(xp, yp, &npts, ofp);
				count = buffer[i++];	/* Draw Text */
				hjust = buffer[i++];
				vjust = buffer[i++];
				x = buffer[i++];
				y = buffer[i++];
				angle = ((float) buffer[i++]) / 10;
				ToPoints(x, y, &fx, &fy, &gToF);

				/*
				 * horizontal justification
				 * 
				 * hjust = 1 => left
				 * 
				 * = 2 => center
				 * 
				 * = 3 => right
				 * 
				 * 
				 * vertical justification
				 * 
				 * vjust = 1 => bottom
				 * 
				 * = 2 => center
				 * 
				 * = 3 => top
				 * 
				 */

				/* 0.33 from trial and error */
				if (vjust == kCenter)
					fy -= 0.33 * fontsize;
				if (vjust == kTop)
					fy -= fontsize;
					

				/* unpack the string from the buffer */
				y = buffer[i++];
				for (x = 0; x < y; x += 2) {
					str[x] = ((char *) buffer)[2 * i];
					str[x + 1] = ((char *) buffer)[2 * i + 1];
					i++;
				}
				str[y] = '\0';

				/* output the text command in AI postscript */
				fprintf(ofp, "u\n0 O\n0 g\n");
				fprintf(ofp, "/_Helvetica %d %d 0 %d z\n", fontsize, fontsize, hjust - 1);

				/*
				 * rotate the text if the angle is more than
				 * 0.05 degrees
				 */
				if (fabs(angle) > 0.05) {
					a = cos(angle * deg_to_rad);
					b = sin(angle * deg_to_rad);
					fprintf(ofp, "[%.4f %.4f %.4f %.4f %.2f %.2f]e\n", a, b, -b, a, fx, fy);
				} else
					fprintf(ofp, "[1 0 0 1 %.2f %.2f]e\n", fx, fy);	/* unrotated */

				fprintf(ofp, "%d (%s)t\nT\n\U\n", y, str);
				break;
			case -6:
				PaintPath(xp, yp, &npts, ofp);
				count = buffer[i++];
				gOldX = 0;
				gOldY = 0;
				x = buffer[i++];
				y = buffer[i++];
				fontsize = (int) ((float) y / 53.3333); /* 53.3333 from trial and error */
				break;
			case -7:
				PaintPath(xp, yp, &npts, ofp);
				count = buffer[i++];	/* Change the linestyle */
				if (buffer[i] <= 0)
					buffer[i] = 1;
				if (gLineType == 't') {
					fprintf(ofp, "%s\n", gLineThicknesses[buffer[i++] - 1]);
				} else {
					fprintf(ofp, "%s\n", gLineDashes[buffer[i++] - 1]);
				}

				break;
			case -8:
				PaintPath(xp, yp, &npts, ofp);
				count = buffer[i++];
				if (count == 1) {
					gXfactor *= (buffer[i] / 1000);
					gYfactor *= (buffer[i++] / 1000);
				}
				if (count == 2) {
					gXfactor *= (buffer[i++] / 1000);
					gYfactor *= (buffer[i++] / 1000);
				}
				break;
			case -10:
				PaintPath(xp, yp, &npts, ofp);

				gStroke_or_Fill = kFill;
				count = buffer[i++];
				igray = buffer[i++];

				if (igray == 0)
					gGrayValue = 1.0;
				if (igray == 1)
					gGrayValue = 0.8;
				if (igray == 2)
					gGrayValue = 0.0;

				fprintf(ofp, "%.1f g\n", gGrayValue);


				break;

			default:
				count = buffer[i++];
				i += count;
			}
	}
}

/*
 * GetShort - read a short integer from a stream
 */
short
GetShort(fp)
	FILE           *fp;
{
	int             hi;

	hi = getc(fp);
	return ((hi << 8) + getc(fp));
}


CopyProlog(ofp, headerType)
	FILE           *ofp;
	char            headerType;
{
	FILE           *fd;
	int             c;
	int             len, lenaux, lenfile;
	char           *temp = 0;
	char           *prologfile = 0;


	/* Note: In the following I defined the prolog path relative to 
	   the aux directory even though it is in the inc directory because 
	   I wanted to use the SACAUX environment variable to find the file */

	/* Set the prolog path - $SACAUX/../inc/  */
	if( ( temp = getenv("SACAUX")) == NULL){
	  fprintf(stderr, "ERROR: Environmental variable SACAUX not defined.\n");
	  exit(1);
	}
	else{
	  lenaux = strlen(temp);
	  lenfile = strlen("/../inc/adobe2.00_a.h");
	  len = lenaux + lenfile;
	  prologfile = (char *) calloc(len,sizeof(char) );
	  strcpy(prologfile,temp);
	  if(prologfile[lenaux-1] == '/')
	    strcat(prologfile,"../inc/");
	  else
	    strcat(prologfile,"/../inc/");
	}


	if (headerType == 'l') {
	   strcat(prologfile,"adobe2.00_a.h");
	   /*	   printf("prologfile is: %s \n", prologfile); */
		if ((fd = fopen(prologfile, "r")) == (FILE *) NULL) {
			printf("\nCan't open Prolog File adobe2.00_a.h\n");
			exit(-1);
		} else {
			rewind(fd);
			while ((c = getc(fd)) != EOF)
				putc(c, ofp);
		}
	} else if (headerType == '1') {
	   strcat(prologfile,"adobe1.30_b.h");
	   /* printf("prologfile is: %s \n", prologfile); */
		if ((fd = fopen(prologfile, "r")) == (FILE *) NULL) {
			printf("\nCan't open Prolog File adobe1.30_b.h\n");
			exit(-1);
		} else {
			rewind(fd);
			while ((c = getc(fd)) != EOF)
				putc(c, ofp);
		}
	} else if (headerType == 's') {
		fprintf(ofp, "%%%%BoundingBox:0 0 612 792\n");
		fprintf(ofp, "%%%%TemplateBox:0 0 612 792\n");
		fprintf(ofp, "%%%%EndComments\n");
		fprintf(ofp, "%%%%EndProlog");
	} else if (headerType == 'i') {
		fprintf(ofp, "%%%%BoundingBox:0 0 612 792\n");
		fprintf(ofp, "%%%%EndComments\n");
		fprintf(ofp, "/m {moveto} def\n");
		fprintf(ofp, "/l {lineto} def\n");
		fprintf(ofp, "/S {stroke} def\n");
		fprintf(ofp, "/w {setlinewidth} def\n");
		fprintf(ofp, "/d {setdash} def\n");
		fprintf(ofp, "%%%%EndProlog\n");
	}
}

Initialize(ofp)
	FILE           *ofp;
{
	/* fprintf(ofp,"\n0.3 w"); */
}

AboutSGFtoAI()
{
	fprintf(stderr, "\n\nUSAGE:\n");
	fprintf(stderr, "\sgftoai sgf_file ps_file header_type line_type include_showpage\n\n");
	fprintf(stderr, "The Plot is 6i wide and 4.5i tall.\n\n");
	fprintf(stderr, "header_type is l,1, s, or i (see below)\n");
	fprintf(stderr, "line_type is t or d for thickness or dash\n");
	fprintf(stderr, "include_showpage is y or n (see below)\n");
	fprintf(stderr, "\nIf you are definitely going to put the file into Illustrator\n");
	fprintf(stderr, "  Use header_type = s; for a short header (smaller file to transfer).\n");
	fprintf(stderr, "Use 1 if you are going into Freehand (Adobe 1.1)\n");
	fprintf(stderr, "Use header_type = i; for a short illustrator INDEPENDENT  header.\n");
	fprintf(stderr, "  Color won't work with the independent header\n");
	fprintf(stderr, "The default header is l (for int) and includes the Illustrator Dictionaries.\n");
	fprintf(stderr, "\nInclude a showpage if you are going to send the\n");
	fprintf(stderr, "  the file directly to a laser-printer.\n");
	fprintf(stderr, "Don't include a showpage if you are going to read\n");
	fprintf(stderr, "  the file into illustrator\n");
	fprintf(stderr, "\nDEFAULTS: header = l, line_type = t, include_showpage = n, \n\n");

}

/*
 * function to paint a postscript path.  first point is move'd to, subsequent
 * points are line'd to.  If global variable gStroke_or_Fill indicates
 * stroking the appropriate operator is output and a return is executed.  For
 * fills, after the fill is executed, the graphics state is returned to the
 * default - stroking and black (0 gray value).
 */

PaintPath(x, y, npts, ofp)
	short          *x, *y;
	int            *npts;
	FILE           *ofp;
{
	int             i, gToF;
	float           fx, fy, dx, dy, distance;

	/* you need at least two points to make a path */
	if (*npts <= 1) {
		*npts = 0;
		return;
	}
	ToPoints(x[0], y[0], &fx, &fy, &gToF);

	fprintf(ofp, "%.2f %.2f m\n", fx, fy);

	for (i = 1; i < *npts; i++) {
		ToPoints(x[i], y[i], &fx, &fy, &gToF);
		if (gToF)
			fprintf(ofp, "%.2f %.2f L\n", fx, fy);
	}
	if (gStroke_or_Fill == kStroke)
		fprintf(ofp, "S\n");
	else if (gStroke_or_Fill == kFill) {
		fprintf(ofp, "f\n");
		gStroke_or_Fill = kStroke;
		gGrayValue = 0;
		fprintf(ofp, "0 g\n");
	}
	*npts = 0;
	return;

}

ToPoints(x, y, fx, fy, gToF)
	int             x, y, *gToF;
	float          *fx, *fy;
{
	/*
	 * convert to points and check to make sure that the point moves at
	 * least one unit of plotter resolution.  If it doesn't, don't bother
	 * plotting the point.
	 */

	float           distance;	/* the distance between the present
					 * position and the previous position
					 * in points */


	/* convert from sgf units to points */
	*fx = (float) x *gXfactor + kX_OFFSET;
	*fy = (float) y *gYfactor + kY_OFFSET;

	distance = (*fx - gOldX) * (*fx - gOldX) + (*fy - gOldY) * (*fy - gOldY);
	distance = (float) sqrt((double) distance);
	if (distance >= minDist)
		*gToF = 1;
	else
		*gToF = 0;

	if (x == 14 && y == 496) {	/* avoid pen move back to origin */
		*gToF = 0;	/* after the plot is finished */
		/* for the BoundingBox calculation */

	} else {

		/* calculate the extrema for the BoundingBox */

		if (*fx >= gXmax)
			gXmax = *fx;
		if (*fy >= gYmax)
			gYmax = *fy;
		if (*fx <= gXmin)
			gXmin = *fx;
		if (*fy <= gYmin)
			gYmin = *fy;
	}

	/*
	 * Store the current position for the next distance calculation
	 */

	gOldX = *fx;
	gOldY = *fy;
}


/* eof */
