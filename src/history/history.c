/* This module implements a history service patterned after the csh history
command. There is one exported function:
 
char *AddToHistory(char *line).
 
AddToHistory is called from sac.c after a command line has been received, but
before any parsing. Any command line which does not begin with "!", "!!", or
"history" is simply passed back to sac. If the command line begins with the string
"history" then the current history list is printed. If the command line begins
with "!!" then any text following "!!" is concatenated to the last command in
the history list and the result is passed back to sac. If the command line
begins with "!" then the behavior of AddToHistory depends on the text immediately
after "!". If the text is a positive number corresponding to a command number
in the history list, then the remainder of the text is appended to the numbered
command and the result is passed back to sac. If the text is a negative number
then a selection is made relative to the last command in the list. If the text
begins with an alphabetic character, then the history list is searched for a
command with a substring matching the text, and if found, that command string
is returned to sac.

The history list is implemented as a linked list with members of type
struct CmdList {
   int num;
   char *cmd;
   struct CmdList *next;
};
The list is not allowed to grow in size indefinitely. When it reaches MAXEVENTS
elements in size, elements from the beginning of the list are recycled to the
end of the list. Thus, only the last MAXEVENTS are available. */




#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "../../inc/history.h"


struct CmdList {		/* basic element of the history list.*/
   int num;
   char *cmd;
   struct CmdList *next;
};

#define NULLLIST (struct CmdList *) 0
#define NULLSTRING '\0'
#define MAXEVENTS 100
static struct CmdList *head = NULLLIST;
static struct CmdList *tail = NULLLIST;
static int Nevents=0;		/* number of elements added so far */






/* ************** Internal functions ************** */
static char * RemoveLeadingBlanks(line) /* from input command line */
char *line ;
{
   while(isspace(*line)) {
      line++;
   }
      return line;   
}


static void RemoveTrailingBlanks(line) /* from input command line */
char *line ;
{
   int j;
   j=strlen(line)-1;
   while(j >= 0)
      if(line[j] == ' '){
         line[j] = NULLSTRING;
         j--;
      }
      else
         break;
 }



static char *RepeatCommand(line) /* repeat last command in list */
char *line ;
{
   char *CombinedCmds;
   int cmdLen;
   line=line+2;
   if(tail == NULLLIST){  /* There are no commands so strip characters and return */
       return line;
   }
   else{
      cmdLen=strlen(tail->cmd) + strlen(line);
      CombinedCmds = (char *) malloc(cmdLen + 1 );
      strcpy(CombinedCmds,tail->cmd);
      CombinedCmds[strlen(tail->cmd)]=NULLSTRING;
      strcat(CombinedCmds,line);
      CombinedCmds[cmdLen]=NULLSTRING;
      return CombinedCmds;
   
   }

}



static char *getCmdString(cmdNum) /* get a command string from list given its number */
int cmdNum ;
{
  struct CmdList *cur;
   cur=head;
   while( cur != NULLLIST){
      if( cmdNum == cur->num){
         return cur->cmd;
      }
      cur=cur->next;
   }
   return NULLSTRING;
}



static int getCmdNum(line) /* return a number at start of string */
char *line ;
{
   int num;
   if(sscanf(line,"%i",&num) != 1){
      printf("Invalid! \n");
      num=0;
      return num;
   }
   
   /* strip off leading digits to leave just any appended command string */
    while( isdigit(*line) || *line == '-'){
      *line=' ';
      line++;
   }
   strcpy(line,RemoveLeadingBlanks(line) );
    
   if(num > 0 )
      return num;
   else
      return num + tail->num + 1;

}



static int getCmdMatch(line) /* return number of command matching string */
char *line ;
{
   struct CmdList *cur;
   int lastmatch=-1;
   cur=head;
   while( cur != NULLLIST){
      if( ! strncmp(cur->cmd, line , strlen(line) ) ){
         lastmatch=cur->num;
      } 
      cur=cur->next;
   }
   return lastmatch;
}
   



static char *RepeatNumberedCommand(line)
char *line ;
/* This routine is called if the command line begins with a "!" */
{
   char *CombinedCmds, *Cmd;
   int cmdLen;
   int cmdNum;
   
   line=line+1;
   if(tail == NULLLIST){  /* There are no commands so strip characters and return */
       printf("No Commands in list! \n");
       line[0]=NULLSTRING;
       return line;
   }
   else if(isalpha(line[0])){
      cmdNum=getCmdMatch(line); /* find matching command in history list */
      if (cmdNum < 1){
         printf("Event not found \n");
         line[0]= NULLSTRING;
         return line;
      }
      if(getCmdString(cmdNum) != NULLSTRING){
         Cmd=getCmdString(cmdNum);
         cmdLen=strlen(Cmd) + strlen(line);
         Cmd[cmdLen]=NULLSTRING;
         return Cmd;
      }
   }
   else{
      cmdNum=getCmdNum(line); /* Get command number and strip that off the line*/
      if(getCmdString(cmdNum) != NULLSTRING){
         Cmd=getCmdString(cmdNum);
         cmdLen=strlen(Cmd) + strlen(line);
         CombinedCmds = (char *) malloc(cmdLen + 1);
         strcpy(CombinedCmds,Cmd);
         CombinedCmds[strlen(Cmd)]=NULLSTRING;
         strcat(CombinedCmds,line);
         CombinedCmds[cmdLen]= NULLSTRING;
         return CombinedCmds;
      }
      else{
         printf("Command not found! \n");
         line[0]= NULLSTRING;
         return line;
      }
   }
   return line;
}



char *AddToList(line)
char *line ;
/* This routine adds the current command to list */
{
   struct CmdList *new, *tmp;
   
   if(++Nevents > MAXEVENTS){ /* List is too big: recycle first element in list */
      tmp=head->next;
      head->num=tail->num+1;
      head->next=NULLLIST;
      tail->next=head;
      tail=head;
      head=tmp;
      free(tail->cmd);
      tail->cmd= (char *) malloc(strlen(line) + 1 );
      strcpy(tail->cmd,line);
      tail->cmd[strlen(line)]=NULLSTRING;
      return(tail->cmd);
   }
   
   
   
   new= (struct CmdList *) malloc( sizeof(struct CmdList));
   if(new == NULLLIST){
      printf("malloc failed in history \n");
      exit(1);
   }
   
   if(head == NULLLIST){	/* start of list */;
      new->next=NULLLIST;
      new->cmd= (char *) malloc(strlen(line) + 1 );
      strcpy(new->cmd,line);
      new->cmd[strlen(line)]=NULLSTRING;
      new->num=1;
      head = tail = new;
      return (new->cmd);
   }
   else{			/* Add to list */
      
      new->num=tail->num+1;
      new->next=NULLLIST;
      new->cmd= (char *) malloc(strlen(line)+1);
      strcpy(new->cmd,line);
      new->cmd[strlen(line)]=NULLSTRING;
      tail->next=new;
      tail=new;
      return (new->cmd);
   }
}




static void PrintHistory()
{
   struct CmdList *cur;
   cur=head;
   printf(" \n");
   while( cur != NULLLIST){
      printf("%6i  %s \n",cur->num, cur->cmd);
      cur=cur->next;
   }
   return;

}




/* ************** Exported functions ************** */

char *AddToHistory(line)
char *line ;
{
   strcpy(line,RemoveLeadingBlanks(line) );
   RemoveTrailingBlanks(line);
   if(strlen(line) == 0)
      return line;
      
   if(line[0] == '!' && line[1] == '!'){
      strcpy(line,RepeatCommand(line) );
      if(strlen(line) > 0){
         printf("%s \n ",line);
         if( (! strncmp("history",line,7)) ){
            PrintHistory();
            strcpy(line,AddToList(line) );
            strcpy(line,"       ");
            line[7]=NULLSTRING;
         }
         else
            strcpy(line,AddToList(line) );         
      }
      return line;
   }
   else if(line[0] == '!'){
      strcpy(line,RepeatNumberedCommand(line) );
      if(strlen(line) > 0){
         if( (! strncmp("history",line,7)) ){
            PrintHistory();
            strcpy(line,AddToList(line) );
            strcpy(line,"       ");
            line[7]=NULLSTRING;
         }
         else
            strcpy(line,AddToList(line) );         
      }
      return line;
   }
   else if( (! strncmp("history",line,7)) ){
      PrintHistory();
      strcpy(line,AddToList(line) );
      strcpy(line,"       ");
      line[7]=NULLSTRING;
      return line;
   }
   
   else  /* history cannot process this line so pass it on */
      strcpy(line,AddToList(line) );
      return line;
      
}
