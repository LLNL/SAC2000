
/*
 * SccsId:	@(#)include_src/css_general.h	106.1	05/15/96
 */

#ifndef _CSS_GENERAL_INCLUDE
#define _CSS_GENERAL_INCLUDE

/*
 * general definitions, used by everyone
 * Required by library, libresponse
 */

#define EOS		'\0'		/* end of string */
#define ERR		-1		/* general error; often system error*/
#define ERR2            -2              /* application error, perror useless */
#define ERR3		-3		/* SQL error use dberror et. al. */
#define ERR_SQL		ERR3		/* SQL error use dberror et. al. */
#ifndef FALSE
#define FALSE		0		/* general no */
#endif
#define NO		0		/* general no */
#define OK		0		/* okay exit */
#ifndef TRUE
#define TRUE		1		/* general yes */
#endif
#define YES		1		/* general yes */

/* lower-case string */
#define TOLCASE(S) { \
	register char	*xx_S; \
	for (xx_S = S;*xx_S;++xx_S) \
		if (isupper(*xx_S)) \
			*xx_S = tolower(*xx_S); \
}

/* upper-case string */
#define TOUCASE(S) { \
	register char	*xx_S; \
	for (xx_S = S;*xx_S;++xx_S) \
		if (islower(*xx_S)) \
			*xx_S = toupper(*xx_S); \
}

#endif /* !_CSS_GENERAL_INCLUDE */
/* all additions should be above the preceding line */
