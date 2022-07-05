#include <stdlib.h>
#include <stdio.h>
#include <string.h>


char *subscpy(to,start,end,len,from)
char *to, *from;
int start, end, len;
{
	/* null terminates the target string, the end of
	   the target string is determined from "to+len"! ! !
	   The substring bounds assume C range (i.e., start at 0).
	 */
	char *t;
	int l;
	int n, ncopy, fromlen, tolen;


	if( to == NULL || from == NULL ) return( NULL );

        if (start < 0) start = 0;
        
        if ((end < 0) || (end > len-1) ) end = len-1;

        n = end - start + 1;
        if ( n < 0 ) n = 0;

        fromlen = strlen(from);

        tolen = strlen(to);

        if (tolen < start) memset(to+tolen, (int)' ', start-tolen);

        if ( fromlen < n ) {
           ncopy = fromlen;
           memset(to+start+ncopy, (int)' ', n-ncopy);
        }
        else  ncopy = n;

        memcpy(to+start,from,ncopy);
        to[len] = '\0';

	return( to );
}
