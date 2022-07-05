/*
CDOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 *** Header:	stdu.h						D. Trimmer
 
 *** Purpose:	Include file which declares functions in usrlib and defines
		commonly used values (line length, yes, no, etc.)
 
 *** History:	07/16/84	File begun--D. Trimmer
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
CEND
*/
 
#include <stdio.h>
 
extern int getfline();

extern int strappend();
extern int strcopy();
extern int strnumat();

 
/*	macro expansions	*/
#define tolower(c)	((c>='A' && c<='Z') ? (c+('a'-'A')) : c)
#define toupper(c)	((c>='a' && c<='z') ? (c-('a'-'A')) : c)
 
#define TRUE	1
#define FALSE	0
#define SET	1
#define CLEAR	0
#define YES	1
#define NO	0
 
#define LINLEN 80
 
#define LONGBYTES 4
#define SHORTBYTES 4
#define FLOATBYTES 4
#define DOUBLEBYTES 8
 
#define ADDRESS_TYPE int

