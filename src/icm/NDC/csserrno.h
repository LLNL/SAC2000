
/*
 * SccsId:	@(#)include_src/csserrno.h	106.1	05/15/96
 */

/*
** This file defines the CSS error codes used by CSS library routines.
** The error strings are in the array css_errlist to allow flexible
** formatting. Variable css_nerr contains the number of css_errlist
** entries and the csserrno should be checked against it before
** de-referencing the css_errlist item to avoid array out-of-bounds errors.
** Convenience function csserror() will invoke perror() for UNIX system
** errors or print the css_errlist string on stderr for CSS application
** errors depending on the value of errtype.
 * Required by library, libresponse
*/

#ifndef CSS_ERRNO_H
#define CSS_ERRNO_H

/* the following error codes are valid only for CSS software errors */
#define CSSNOERR	0	/* unused, no CSS error */
#define CSSEFRMT	1	/* Bad external file format */
#define CSSERELTYPE	2	/* No such relation type */
#define CSSEDOM		3	/* Detected argument out of range of function */
#define CSSESCAN	4	/* Couldn't match conversion requested */
#define CSSEUNEOF	5	/* Unexpected EOF */

#ifndef CSSERRDEF
extern int csserrno;
extern char *css_errlist[];
extern int css_nerr;

void csserror(/* char *s, int errtype */);
void setcsserrno(/* int errcode */);
char *nondbgeterror(/* int errtype */);
/* s is any string supplied by the user;
   errcode is one of the CSS error codes listed above;
   errtype is error type code returned by the CSS library function */
#endif /* CSSERRDEF */

/* don't add anything below the next line */
#endif /* CSS_ERRNO_H */
