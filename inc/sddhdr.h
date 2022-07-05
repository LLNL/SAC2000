/* ../../inc/sddhdr.h */

#define	MBSHDR	716
#define	MSCOM	10
#define	MSREP	150
#define	MWESHD	(MSREP + 12)
#define	MWSHDR	179



struct t_kmshdr {
	char kshdr[MBSHDR][9];
	}	kmshdr;


#ifdef DOINITS
int *const iscalg =  (int*) &kmshdr.kshdr[0] + 112;
int *const isclas = 0; /*(int*)kmshdr.kshdr;*/
int *const iscom = 0; /*(int*)((char*)kmshdr.kshdr + 40 ;*/
int *const isdate = 0; /*(int*)((char*)kmshdr.kshdr + 80 ;*/
int *const isdelt = 0; /*(int*)((char*)kmshdr.kshdr + 88 ;*/
int *const isfrmt = 0; /*(int*)((char*)kmshdr.kshdr + 4 ;*/
int *const ishdr = 0; /*(int*)kmshdr.kshdr;*/
int *const isnpts = 0; /*(int*)((char*)kmshdr.kshdr + 92 ;*/
int *const isrep = 0; /*(int*)((char*)kmshdr.kshdr + 116 ;*/
int *const issdep = 0; /*(int*)((char*)kmshdr.kshdr + 108 ;*/
int *const issel = 0; /*(int*)((char*)kmshdr.kshdr + 96 ;*/
int *const issla = 0; /*(int*)((char*)kmshdr.kshdr + 100 ;*/
int *const isslo = 0; /*(int*)((char*)kmshdr.kshdr + 104 ;*/
int *const istime = 0; /*(int*)((char*)kmshdr.kshdr + 84 ;*/
char *const kschan = 0; /*(char*)((char*)kmshdr.kshdr + 28 ;*/
char *const kschdr = 0; /*(char*)kmshdr.kshdr;*/
char *const ksclas = 0; /*(char*)kmshdr.kshdr;*/
char *const kscom = 0; /*(char*)((char*)kmshdr.kshdr + 40 ;*/
char *const ksevnm = 0; /*(char*)((char*)kmshdr.kshdr + 12 ;*/
char *const ksfrmt = 0; /*(char*)((char*)kmshdr.kshdr + 4 ;*/
char *const ksstnm = 0; /*(char*)((char*)kmshdr.kshdr + 20 ;*/


/* int *const Iscom = &iscom[0] - 1; */
int *const Iscom = 0; /*(int*)((char*)kmshdr.kshdr + 40 - 4 ;*/

/* int *const Ishdr = &ishdr[0] - 1; */
int *const Ishdr = 0; /*(int*)((char*)kmshdr.kshdr - 4 ;*/

/* int *const Isrep = &isrep[0] - 1; */
int *const Isrep = 0; /*(int*)((char*)kmshdr.kshdr + 116 - 4 ;*/

#else
extern int *const iscalg;
extern int *const isclas;
extern int *const iscom;
extern int *const isdate;
extern int *const isdelt;
extern int *const isfrmt;
extern int *const ishdr;
extern int *const isnpts;
extern int *const isrep;
extern int *const issdep;
extern int *const issel;
extern int *const issla;
extern int *const isslo;
extern int *const istime;
extern char *const kschan;
extern char *const kschdr;
extern char *const ksclas;
extern char *const kscom;
extern char *const ksevnm;
extern char *const ksfrmt;
extern char *const ksstnm;
extern int *const Iscom;
extern int *const Ishdr;
extern int *const Isrep;
#endif


