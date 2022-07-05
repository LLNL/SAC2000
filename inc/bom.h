/* ../../inc/bom.h */

#include "mach.h"
#define	MBFL	MDFL



struct t_cmbom {
	int nbfl, ibflc, nlenbf, ndxhbf, ndx1bf, ndx2bf;
	int lnewhdr ;	/* optionally let the header information come from
			   the new file being added or whatever in
			   addf, subf, mulf, and divf. maf 990526 */
	}	cmbom;
struct t_kmbom {
	char kbfl[MAXCHARS], kecnpt[9], kecdel[9];
	}	kmbom;


