#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
double /*FUNCTION*/ taper(freq, fqh, fql)
double freq, fqh, fql;
{
	double dblepi, taper_v;
	static double twopi = 6.283185307179586;

	/* SUBROUTINE TO TAPER SPECTRA BY A COSINE
	 *
	 * CALLING ARGUMENTS:
	 *
	 *     FREQ - FREQUENCY IN QUESTION
	 *     FQH - FREQUENCY AT WHICH THERE IS A TRANSITION BETWEEN UNITY AND
	 *           THE TAPER
	 *     FQL - FREQUENCY AT WHICH THERE IS A TRANSITION BETWEEN ZERO AND THE
	 *           TAPER
	 *     NOTE:  IF FQL>FQH   LO-PASS
	 *            IF FQH>FQL   HI-PASS
	 * */

	dblepi = 0.5e0*twopi;
	if( fql > fqh )
		goto L_210;
	if( fqh > fql )
		goto L_510;
        fprintf(MUNOUT," INVALID WINDOW SPECIFIED\n");
	return( taper_v );

	/* LO-PASS CASE
	 * */
L_210:
	if( freq < fqh )
		taper_v = 1.0e0;
	if( freq >= fqh && freq <= fql )
		taper_v = 0.5e0*(1.0e0 + cos( dblepi*(freq - fqh)/(fql - fqh) ));
	if( freq > fql )
		taper_v = 0.0e0;

	return( taper_v );

	/* HI-PASS CASE
	 * */
L_510:
	if( freq < fql )
		taper_v = 0.0e0;
	if( freq >= fql && freq <= fqh )
		taper_v = 0.5e0*(1.0e0 - cos( dblepi*(freq - fql)/(fqh - fql) ));
	if( freq > fqh )
		taper_v = 1.0e0;

	return( taper_v );

} /* end of function */

