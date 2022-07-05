/*
 * Copyright 1994 Science Applications International Corporation.
 *
 * NAME
 *	cssgeterror
 *
 * FILE 
 *	csserror.c
 *
 * SYNOPSIS
 *
 *	char *cssgeterror (int error_code) 
 *
 *	int setcsserrno (int code)
 *
 *	void csserror (char *s, int t)
 *
 *	void perror ()
 *
 * DESCRIPTION
 *
 * cssgeterror - A function to return UNIX system or Center application
 * library error message text for building the third argument to the generic
 * error funtion passed to Center application library or database library
 * routines.  Error_code is either ERR or ERR2 defined in css_general.h to
 * indicate that this is either a UNIX system error or a application detail
 * error.  This routine should be called by Center application library routines
 * for non-database errors to retrieve the appropriate error message string.
 *
 * DIAGNOSTICS
 *
 * FILES
 *
 * NOTES
 * 
 * SEE ALSO
 *
 * AUTHOR
 *
 */

#ifndef	lint
static	char	SccsId[] = "@(#)libresponse/csserror.c	105.1	05/29/96 Copyright 1994 Science Applications International Corporation.";
#endif


#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "css_general.h"
#define CSSERRDEF 1	/* This is used by csserrno.h */
#include "csserrno.h"
#include "libresponse.h"

/* Following are the Center application error variables */

char *css_errlist[] = 
	{
	"No CSS error",
	"Bad external file format",
	"No such relation type",
	"Detected argument out of range of function",
	"Couldn't match conversion requested",
	"Unexpected EOF",
	};
int css_nerr = sizeof(css_errlist)/sizeof(char *);
int csserrno;	/* Application error code set by Center library routine */

#define UNKSYSERR "Unknown UNIX system error code"
#define UNKCSSERR "Unknown CSS application library error code"
#define UNKERRTYPE "Unknown error type"

static char default_text[100];	/* Storage for unknown error text */

/* -
 * cssgeterror - A function to return UNIX system or Center application
 * library error message text for building the third argument to the generic
 * error funtion passed to Center application library or database library
 * routines.  Error_code is either ERR or ERR2 defined in css_general.h to
 * indicate that this is either a UNIX system error or a application detail
 * error.  This routine should be called by Center application library routines
 * for non-database errors to retrieve the appropriate error message string.
 */

char *
cssgeterror (error_code)
int	error_code;		/* ERR or ERR2 defined in css_general.h */
{
	char	*err_text;	/* pointer to return error text */

	switch (error_code) 
	{
	case ERR:		/* UNIX system error */
		if (errno >= 0)
			err_text = strerror(errno);
		else
		{
			sprintf (default_text, "%s %d", UNKSYSERR, errno);
			err_text = default_text;
		}
		break;
	case ERR2:		/* Center application error */
		if (csserrno < css_nerr)
			err_text = css_errlist[csserrno];
		else
		{
			sprintf (default_text, "%s %d", UNKCSSERR, csserrno);
			err_text = default_text;
		}
		break;
	default:
		{
			sprintf (default_text,"%s %d", UNKERRTYPE, error_code);
			err_text = default_text;
		}
		break;
	}
	return err_text;
}


void
setcsserrno (code)
int	code;
{
	csserrno = code;
}


void 
csserror (s, t)
char	*s;			/* character string for beginning of message */
int	t;			/* error message type received
				 * ERR => UNIX ERROR, ERR2 => CSS error */
{
	switch (t) 
	{
	case ERR:
		perror (s);
		break;
	case ERR2:
		if (csserrno > sizeof(css_errlist)/sizeof(char *))
			fprintf (stderr, "%s: unknown CSS error code %d\n",
					 s, csserrno);
		else
			fprintf (stderr, "%s: %s\n", s, css_errlist[csserrno]);
		break;
	default:
		fprintf (stderr, "%s: unknown CSS error message type %d\n", 
			 s, t);
		break;
	}
}

