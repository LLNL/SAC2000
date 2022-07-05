#include <stdlib.h>
#include <stdio.h>
#include "../../inc/EVRESPnames.h"
#include <string.h>

/* This module implements a set of access functions that allow the
   SAC user to specify STATION, CHANNEL, NETWORK, DATE, TIME, TYPE, and LOCID keywords 
   that override the default values read from the SAC headers.  12 static
   structures store information as to whether a particular keyword
   has been encountered, and if so, the value of the following token.
   seven routines are provided to set values in those structures, and
   seven routines are provided to retrieve values. One routine is provided
   to check if a particular structure has been set, and one routine is
   provided to clear/initialize all the structures. A pair of functions
   (setTransferDirection, getTransferDirection) are provided so that the
   evresp function can know whether it is currently calculating the TO or
   FROM transfer function without passing that information in the argument
   lists. 
   
   
    
   Because the data are static, the rest of the program can only access the 
   data using the access functions. The access functions are:

********************* Access Functions *****************************   
   void setStationName(char *, enum Direction);
   void setNetworkName(char *, enum Direction);
   void setChannelName(char *, enum Direction);
   void setLocidName(  char *, enum Direction);

   char *getStationName(enum Direction);
   char *getNetworkName(enum Direction);
   char *getChannelName(enum Direction);
   char *getLocidName(  enum Direction);

   int isSet(enum EVparam, enum Direction);
   void clearEVRESPstrucs(void);

   void setTransferDirection(int);
   int getTransferDirection(void);
   
   void setDate(char *, enum Direction);
   void setTime(char *, enum Direction);
   int getYear(enum Direction);
   int getJday(enum Direction);
   int getHour(enum Direction);
   int getMinute(enum Direction);
   int getSecond(enum Direction);

   void setType(char *, enum Direction);
   char *getType(enum Direction);
   
*********************************************************************
   
   eleven keywords are recognized:
   	STATION
   	CHANNEL
	LOCID
   	NETWORK
   	DATE			use date following keyword in yyyy/ddd format
   	TIME			use time following keyword in hh:mm:ss format
   	TO			Transfer to this (STATION,CHANNEL,NETWORK)
   	FROM			Transfer from this (STATION,CHANNEL,NETWORK)
   	TYPE(disabled)			Type VEL, DIS, ACC
        FILENAME
	NAME_FROM_DB


   To use these access functions, include the header file EVRESPnames.h.	
   The functions are intended to be used in the following manner. 
   
   In xtransfer.c begin the function with a call to
   clearEVRESPstrucs();
   
   Otherwise, previous calls to TRANSFER may influence the results of
   the current call since they may have set some of the structures.
   
   Before calling getins execute setTransferDirection with the desired direction:
   setTransferDirection(FROM);
   
   
   

   In function getins the keywords STATION, CHANNEL, and NETWORK are parsed.
   Assume that STATION was found. Then KP(1,0) holds the requested station
   name. The following code fragment shows how to set the station name.
   
      else if( lkchar( "STATION$",9, MCPFN, KP(1,0),kp_s, &nchar ) ){
          setStationName(KP(1,0), getTransferDirection());
          
   Repeat for CHANNEL, NETWORK...
   
   In the EvrespGateway function the network string must be set. Use the following code:
   
   if(isSet(NETWORK, getTransferDirection())){
    strcpy(net_code,getNetworkName(getTransferDirection()));
    
    
    Finally, in the evresp function set the channel and station strings as required:
    
 if(isSet(STATION, getTransferDirection()))
    strcpy(station,getStationName(getTransferDirection()));
 else
    strcpy(station,stalst);
 

 if(isSet(CHANNEL, getTransferDirection()))
    strcpy(channel,getChannelName(getTransferDirection()));
 else
    strcpy(channel,chalst);
 NOTE: chalst and stalst are globals read from the sac header
   
*/ 

  
 /* **************** Static data used in this module ********************* */  
struct channel {
   char *name;
   int set;
};
static struct channel chanTo, chanFrom;

struct Station {
   char *name;
   int set;
};
static struct Station statTo, statFrom;

struct Network {
   char *name;
   int set;
};
static struct Network netTo, netFrom;

struct locid {
   char *name;
   int set;
};
static struct locid locidTo, locidFrom;

struct Date {
   int year;
   int jday;
   int set;
};
static struct Date dateTo, dateFrom;

struct Time {
   int hour;
   int min;
   int sec;
   int set;
};
static struct Time timeTo, timeFrom;

struct Type {
   char *type;
   int set;
};
static struct Type typeTo, typeFrom;



struct Filename {
   char *name;
   int set;
};
static struct Filename nameTo, nameFrom;


/* direction of current transfer */
static int Direction;


static int GetNameFromDB;

/* *********************************************************************** */

void deblank ( char *strg ) ;

/* use the deblank that's in EvrespGateway */

/* private function for use inside module *
#ifdef __STDC__
static void  deblank(char *strg)
#else
static void  deblank(strg)
char *strg ;
#endif
{
  
  int i;
  for (i=0;i<strlen(strg);i++)
     if(strg[i] == ' ')
        strg[i]='\0';
}

*/




#ifdef __STDC__
void clearEVRESPstrucs(void) /* Set all data structures in module to default values */
#else
void clearEVRESPstrucs()
#endif
{
   free(chanTo.name);
   free(chanFrom.name);
   free(statTo.name);
   free(statFrom.name);
   free(locidTo.name);
   free(locidFrom.name);
   free(netTo.name);
   free(netFrom.name);
   free(nameTo.name);
   free(nameFrom.name);


   chanTo.name    = 0;
   chanTo.set     = 0;
   chanFrom.name  = 0;
   chanFrom.set   = 0;
   statTo.name    = 0;
   statTo.set     = 0;
   statFrom.name  = 0;
   statFrom.set   = 0;
   locidTo.name   = 0;
   locidTo.set    = 0;
   locidFrom.name = 0;
   locidFrom.set  = 0;
   netTo.name     = 0;
   netTo.set      = 0;
   netFrom.name   = 0;
   netFrom.set    = 0;
   nameFrom.name  = 0;
   nameFrom.set   = 0;
   nameTo.name    = 0;
   nameTo.set     = 0;

   timeTo.hour    = 0;
   timeTo.min     = 0;
   timeTo.sec     = 0;
   timeTo.set     = 0;
   timeFrom.hour  = 0;
   timeFrom.min   = 0;
   timeFrom.sec   = 0;
   timeFrom.set   = 0;
   dateTo.year    = 0;
   dateTo.jday    = 0;
   dateTo.set     = 0;
   dateFrom.year  = 0;
   dateFrom.jday  = 0;
   dateFrom.set   = 0;

   typeTo.type    = 0;
   typeTo.set     = 0;
   typeFrom.type  = 0;
   typeFrom.set   = 0;
   
   Direction      = FROM;
   GetNameFromDB  = 0;
}


#ifdef __STDC__
int isSet(enum EVparam request, enum Direction dir)
#else
int isSet(request, dir)
enum EVparam request ;
enum Direction dir ;
#endif
{   /* See if requested parameter has been set, and if so, in what direction */
   switch(request){
      case CHANNEL:
         if(dir == TO)
            return chanTo.set;
         else if(dir == FROM)
            return chanFrom.set;
         else
            return FALSE;
      
      case STATION:
         if(dir == TO)
            return statTo.set;
         else if(dir == FROM)
            return statFrom.set;
         else
            return FALSE;

      case LOCID:
         if(dir == TO)
            return locidTo.set;
         else if(dir == FROM)
            return locidFrom.set;
         else
            return FALSE;
      
      case NETWORK:
         if(dir == TO)
            return netTo.set;
         else if(dir == FROM)
            return netFrom.set;
         else
            return FALSE;

      case DATE:
         if(dir == TO)
            return dateTo.set;
         else if(dir == FROM)
            return dateFrom.set;
         else
            return FALSE;

      case TIME:
         if(dir == TO)
            return timeTo.set;
         else if(dir == FROM)
            return timeFrom.set;
         else
            return FALSE;

/*      case TYPE:
         if(dir == TO)
            return typeTo.set;
         else if(dir == FROM)
            return typeFrom.set;
         else
            return FALSE;	*/

      case FILENAME:
         if(dir == TO)
            return nameTo.set;
         else if(dir == FROM)
            return nameFrom.set;
         else
            return FALSE;

      case NAME_FROM_DB:
         if(dir == FROM)
            return GetNameFromDB;
         else
            return 0;
      
      default:
         return 0;
   }

}
/* ------------------------------------------------------- */






#ifdef __STDC__
void setType(char *type, enum Direction dir)
#else
void setType(type, dir)
char *type ;
enum Direction dir ;
#endif
{ /* Set type struct from type */
   int len;
   deblank(type);
   len = strlen(type) + 1;
   
   if(dir == FROM){
      typeFrom.type= (char *) malloc(len);
      strcpy(typeFrom.type,type);
      typeFrom.set=TRUE;
      }
   else if(dir == TO){
      typeTo.type= (char *) malloc(len);
      strcpy(typeTo.type,type);
      typeTo.set=TRUE;
      }
}
/* ------------------------------------------------------- */





#ifdef __STDC__
char *getType(enum Direction dir)
#else
char *getType(dir)
enum Direction dir ;
#endif
{ /* Retrieve type for specified direction */
   if(dir == FROM){
      return typeFrom.type;
      }
   else if(dir == TO){
      return typeTo.type;
      }
      else{
      	 return typeFrom.type;
      }
}
/* ------------------------------------------------------- */







#ifdef __STDC__
void setTime(char *time, enum Direction dir)
#else
void setTime(time, dir)
char *time ;
enum Direction dir ;
#endif
{ /* Set Time struct from time */
   if(dir == FROM){
      if(sscanf(time,"%i:%i:%i ",&(timeFrom.hour),&(timeFrom.min),&(timeFrom.sec)) == 3)
         timeFrom.set=TRUE;
      }
      
   else if(dir == TO){
      if(sscanf(time,"%i:%i:%i ",&(timeTo.hour),&(timeTo.min),&(timeTo.sec)) == 3)
         timeTo.set=TRUE;
      }
}
/* ------------------------------------------------------- */





#ifdef __STDC__
int getHour(enum Direction dir)
#else
int getHour(dir)
enum Direction dir ;
#endif
{ /* Retrieve hour for specified direction */
   if(dir == FROM){
      return timeFrom.hour;
      }
   else if(dir == TO){
      return timeTo.hour;
      }
      else{
      return timeFrom.hour;
   }
}
/* ------------------------------------------------------- */





#ifdef __STDC__
int getMinute(enum Direction dir)
#else
int getMinute(dir)
enum Direction dir;
#endif
{ /* Retrieve minute for specified direction */
   if(dir == FROM){
      return timeFrom.min;
      }
   else if(dir == TO){
      return timeTo.min;
      }
      else{
      return timeFrom.min;
      }
}
/* ------------------------------------------------------- */






#ifdef __STDC__
int getSecond(enum Direction dir)
#else
int getSecond(dir)
enum Direction dir ;
#endif
{ /* Retrieve second for specified direction */
   if(dir == FROM){
      return timeFrom.sec;
      }
   else if(dir == TO){
      return timeTo.sec;
      }
      else{
      return timeFrom.sec;
      }
}
/* ------------------------------------------------------- */






#ifdef __STDC__
void setDate(char *date, enum Direction dir)
#else
void setDate(date, dir)
char *date ;
enum Direction dir ;
#endif
{ /* Set Date struct from date */
   
   if(dir == FROM){
      if(sscanf(date,"%i/%i ",&(dateFrom.year),&(dateFrom.jday)) == 2)
         dateFrom.set=TRUE;
      }
      
   else if(dir == TO){
      if(sscanf(date,"%i/%i ",&(dateTo.year),&(dateTo.jday)) == 2)
         dateTo.set=TRUE;
      }
}
/* ------------------------------------------------------- */





#ifdef __STDC__
int getYear(enum Direction dir)
#else
int getYear(dir)
enum Direction dir ;
#endif
{ /* Retrieve year for specified direction */
   if(dir == FROM){
      return dateFrom.year;
      }
   else if(dir == TO){
      return dateTo.year;
      }
      else{
      return dateFrom.year;
      }
}
/* ------------------------------------------------------- */






#ifdef __STDC__
int getJday(enum Direction dir)
#else
int getJday(dir)
enum Direction dir ;
#endif
{ /* Retrieve day of year for specified direction */
   if(dir == FROM){
      return dateFrom.jday;
      }
   else if(dir == TO){
      return dateTo.jday;
      }
      else{
      return dateFrom.jday;
      }
}
/* ----------------------------------------------------------- */








#ifdef __STDC__
void setStationName(char *name, enum Direction dir)
#else
void setStationName(name, dir)
char *name ;
enum Direction dir ;
#endif
{ /* Set station name to name */
   int len;
   deblank(name);
   len = strlen(name) + 1;
   
   if(dir == FROM){
      statFrom.name= (char *) malloc(len);
      strcpy(statFrom.name,name);
      statFrom.set=TRUE;
      }
   else if(dir == TO){
      statTo.name= (char *) malloc(len);
      strcpy(statTo.name,name);
      statTo.set=TRUE;
      }
}
/* ----------------------------------------------------------- */





#ifdef __STDC__
char *getStationName(enum Direction dir)
#else
char *getStationName(dir)
enum Direction dir ;
#endif
{ /* Retrieve station for specified direction */
   if(dir == FROM){
      return statFrom.name;
      }
   else if(dir == TO){
      return statTo.name;
      }
      else{
      return statFrom.name;
      }
}
/* ----------------------------------------------------------- */





#ifdef __STDC__
void setNetworkName(char *name, enum Direction dir)
#else
void setNetworkName(name, dir)
char *name ;
enum Direction dir ;
#endif
{  /* Set network name to name */
   int len;
   deblank(name);
   len = strlen(name) + 1;
   
   if(dir == FROM){
      netFrom.name= (char *) malloc(len);
      strcpy(netFrom.name,name);
      netFrom.set=TRUE;
      }
   else if(dir == TO){
      netTo.name= (char *) malloc(len);
      strcpy(netTo.name,name);
      netTo.set=TRUE;
      }
     
}      
/* ----------------------------------------------------------- */





#ifdef __STDC__
char *getNetworkName(enum Direction dir)
#else
char *getNetworkName(dir)
enum Direction dir ;
#endif
{   /* Retrieve network name for specified direction */
   if(dir == FROM){
      return netFrom.name;
      }
   else if(dir == TO){
      return netTo.name;
      }
      else{
      return netFrom.name;
      }
}
/* ----------------------------------------------------------- */



#ifdef __STDC__
void setChannelName(char *name, enum Direction dir)
#else
void setChannelName(name, dir)
char *name ;
enum Direction dir ;
#endif
{  /* Set channel name to name */
   int len;
   deblank(name);
   len = strlen(name) + 1;
   
   if(dir == FROM){
      chanFrom.name= (char *) malloc(len);
      strcpy(chanFrom.name,name);
      chanFrom.set=TRUE;
      }
   else if(dir == TO){
      chanTo.name= (char *) malloc(len);
      strcpy(chanTo.name,name);
      chanTo.set=TRUE;
      }
}
/* ----------------------------------------------------------- */





#ifdef __STDC__
char *getChannelName(enum Direction dir)
#else
char *getChannelName(dir)
enum Direction dir ;
#endif
{  /* Retrieve channel name for specified direction */
   char *answer;
   if(dir == FROM){
      answer=chanFrom.name;
      return answer;
      }
   else if(dir == TO){
      answer=chanTo.name;
      return answer;
      }
      else{
      	      return chanFrom.name;
      }
}
/* ----------------------------------------------------------- */





#ifdef __STDC__
void setLocidName(char *name, enum Direction dir)
#else
void setLocidName(name, dir)
char *name ;
enum Direction dir ;
#endif
{ /* Set locid name to name */
   int len;
   deblank(name);
   len = strlen(name) + 1;

   if(dir == FROM){
      locidFrom.name= (char *) malloc(len);
      strcpy(locidFrom.name,name);
      locidFrom.set=TRUE;
      }
   else if(dir == TO){
      locidTo.name= (char *) malloc(len);
      strcpy(locidTo.name,name);
      locidTo.set=TRUE;
      }
}
/* ----------------------------------------------------------- */





#ifdef __STDC__
char *getLocidName(enum Direction dir)
#else
char *getLocidName(dir)
enum Direction dir ;
#endif
{ /* Retrieve locid for specified direction */
   if(dir == FROM){
      return locidFrom.name;
      }
   else if(dir == TO){
      return locidTo.name;
      }
      else{
      	      return locidFrom.name;
      }
}
/* ----------------------------------------------------------- */





#ifdef __STDC__
void setFileName(char *name, enum Direction dir)
#else
void setFileName(type, dir)
char *name ;
enum Direction dir ;
#endif
{ 
   int len;
   deblank(name);
   len = strlen(name) + 1;
   
   if(dir == FROM){
      nameFrom.name = (char *) malloc(len);
      strcpy(nameFrom.name,name);
      nameFrom.set=TRUE;
      }
   else if(dir == TO){
      nameTo.name = (char *) malloc(len);
      strcpy(nameTo.name,name);
      nameTo.set =TRUE;
      }
}
/* ----------------------------------------------------------- */



#ifdef __STDC__
char *getFileName(enum Direction dir)
#else
char *getFileName(dir)
enum Direction dir ;
#endif
{  
   if(dir == FROM){
      return nameFrom.name;
      }
   else if(dir == TO){
      return nameTo.name;
      }
      else{
      	      return nameFrom.name;
      }
}
/* ----------------------------------------------------------- */



#ifdef __STDC__
void setUseDBName(int value, enum Direction dir)
#else
void setUseDBName(type, dir)
int value ;
enum Direction dir ;
#endif
{ 
   if(dir == FROM) GetNameFromDB = value;
}
/* ----------------------------------------------------------- */



#ifdef __STDC__
int getUseDBName(enum Direction dir)
#else
int getUseDBName(dir)
enum Direction dir ;
#endif
{  
   if(dir == FROM)
      return GetNameFromDB;
   else
      return 0;
}
/* ----------------------------------------------------------- */











#ifdef __STDC__
void setTransferDirection(int dir)
#else
void setTransferDirection(dir)
int dir ;
#endif
{
   Direction=dir;
}

#ifdef __STDC__
int getTransferDirection(void)
#else
int getTransferDirection()
#endif
{
   return (Direction);
}



