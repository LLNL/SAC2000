/*
 * SccsId:  @(#)include_src/cdefs.h	106.1	05/15/96
 */

#ifndef _CDEFS_H_
#define _CDEFS_H_	1

/*
 *  The following macros may be used with ansi and non-ansi compilers.  
 *  Currently we support use of this file with C, ANSI C and C++ compilers.
 */

/*
 *  UsePrototypes - macro for checking for strict C/C++ function prototype 
 *                  checking.
 *
 *  Use UsePrototypes instead of checking for __STDC__.
 *  The user may test using either "#if" or "#ifdef" with the same result.  
 */

#ifndef UsePrototypes
#	if defined(__STDC__) || defined(__cplusplus) || defined(c_plusplus)
#		define UsePrototypes 1
#	endif
#endif /* !UsePrototypes */


/*
 *  Proto - macro for defining ANSI C style prototypes with ANIS & non-ANSI 
 *          compilers.
 *
 *  The recommended practice is to use Proto for all function prototypes.
 *
 *	Proto (extern int, func_foo, (int arg1, char *arg2));
 *
 */
#ifndef Proto
#	if UsePrototypes
#		define Proto(type,name,args) type name args
#	else
#		define Proto(type,name,args) type name ()
#	endif
#endif /* !Proto */

#ifndef UseVarargsPrototypes
#	ifdef UsePrototypes
#		define UseVarargsPrototypes 1
#	endif
#endif /* !UseVarargsPrototypes */


/*
 *  VA_START - macro to use rather than va_start().  This 
 *             will allow code to work under either ANSI C 
 *             or K&R C.  The second argument is the last 
 *	       formal argument to the function prior to the 
 *	       start of the varargs list.
 */
#ifndef VA_START
#	if UseVarargsPrototypes
#		include <stdarg.h>
#		define VA_START(a,b) va_start(a,b)
#	else
#		include <varargs.h>
#		define VA_START(a,b) va_start(a)
#	endif
#endif /* VA_START */


/*
 *  CONST, VOLATILE & SIGNED are all supplied to handle special 
 *  argument handling for ANSI C.
 */

#ifndef CONST
#	if UsePrototypes
#		define CONST const
#	else
#		define CONST
#	endif
#endif /* !CONST */

#ifndef VOLATILE
#	if UsePrototypes
#		define VOLATILE volatile
#	else
#		define VOLATILE
#	endif
#endif /* !VOLATILE */

#ifndef SIGNED
#	if UsePrototypes
#		define SIGNED signed
#	else
#		define SIGNED
#	endif
#endif /* !SIGNED */

#endif /*_CDEFS_H_ */
