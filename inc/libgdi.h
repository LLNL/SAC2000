/*
 *  Copyright 1992 Science Applications International Corporation.
 */

/*
 * NAME    
 *	libgdi.h
 *
 * DESCRIPTION
 *      Contains defines and function prototypes for Generic Database
 *	Interface Library.
 *
 * AUTHOR
 *	B. MacRitchie, June 26, 1992
 *
 */

#ifndef _LIBGDI_H_
#define _LIBGDI_H_

#pragma ident "@(#)libgdi/include/libgdi.h	109.1 30 Nov 1995 SAIC"

#ifndef Proto
#ifdef __STDC__
#define Proto(type, name, args)	type name args
#else
#define Proto(type, name, args)	type name ()
#endif
#endif

#include <sys/types.h>

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

#define GDI_DBNULL	-1
#define GDI_NOT_DBNULL	0

#define GDI_MAX_COLUMNS	1000
#define	GDI_MAX_NAME	40
#define GDI_MAX_TYPE	40
#define	GDI_MAX_KEYNAME	15

#define GDI_FETCH_ALL	-1
#define GDI_FETCH_DEFAULT	500

/*=== Array and String Lengths ===*/
#define GDI_SCALAR      0
#define GDI_VARLEN      -1

/*======= dbStatus ========*/
typedef int	dbStatus;
#define GDI_SUCCESS 0
#define GDI_FAILURE 1

/*======= dbDebug =========*/
typedef int dbDebug;
#define GDI_DEBUG_OFF 0
#define GDI_DEBUG_ON  1
#define GDI_DEBUG_VERBOSE 2

/*======= dbCtype ========*/

typedef int dbCtype;

/* The DataTypes structure is used by the Postgres and Montage interfaces
 * to store information about data types in the vendor conn.
 */
typedef struct data_types_struct
{
	char      db_name[GDI_MAX_NAME];
	int       db_type;	/* Database data type code */
	dbCtype   gdi_type;	/* GDI type code  */
	int       str_size;	/* -1 is variable length */
	int       array_size;	/* -1 is variable length, 0 is scalar */
} DataTypes;

#define GDI_UNDEFINED	0
#define GDI_CHAR	1
#define GDI_SHORT	2
#define GDI_INTEGER	3
#define GDI_LONG	4
#define GDI_UCHAR	5
#define GDI_UINTEGER	6
#define GDI_USHORT	7
#define GDI_ULONG	8
#define GDI_FLOAT	9
#define GDI_DOUBLE	10
#define GDI_STRING	11
#define GDI_USER_DEFINED 12
#define GDI_BINARY	13

/*  oracle database data types */
#define GDI_ORA_CHARACTER      1
#define GDI_ORA_VARCHAR2       1
#define GDI_ORA_NUMBER         2
#define GDI_ORA_INT            3
#define GDI_ORA_FLOAT          4
#define GDI_ORA_STRING         5
#define GDI_ORA_LONG           8
#define GDI_ORA_ROWID          11
#define GDI_ORA_DATE           12
#define GDI_ORA_RAW            23
#define GDI_ORA_LONG_RAW       24
#define GDI_ORA_CHAR           96
#define GDI_ORA_MLSLABEL       105
#define GDI_ORA_RAW_MLS_LABEL  106

/*======= dbVendor ========*/
typedef int dbVendor;
#define GDI_UNKNOWN	0
#define GDI_SYBASE	1
#define GDI_ORACLE	2
#define GDI_INGRES	3
#define GDI_POSTGRES	4
#define GDI_MONTAGE	5
#define GDI_ILLUSTRA	6
#define GDI_ODBC        7
#define GDI_ORACLE7	8
#define GDI_SYBASE10	9
#define GDI_ORACLE6	10

#define GDI_VENDOR_SIZE    20
#define	GDI_UNKNOWN_S	   "unknown"
#define	GDI_SYBASE_S	   "sybase"
#define	GDI_SYBASE10_S	   "sybase10"
#define	GDI_ORACLE_S	   "oracle"
#define	GDI_ORACLE6_S	   "oracle6"
#define	GDI_ORACLE7_S	   "oracle7"
#define	GDI_ORACLE_PROC_S  "oracle_proc"
#define	GDI_ORACLE6_PROC_S "oracle6_proc"
#define	GDI_ORACLE7_PROC_S "oracle7_proc"
#define GDI_INGRES_S	   "ingres"
#define GDI_POSTGRES_S	   "postgres"
#define GDI_MONTAGE_S	   "montage"
#define GDI_ILLUSTRA_S	   "illustra"
#define GDI_ODBC_S	   "odbc"

#define GDI_VERSION_SIZE  5

/*======= dbLang ========*/
typedef int dbLang;
#define GDI_SQL		0
#define GDI_POSTQUEL	1

/*======= print format flags: dbFormat ========*/
typedef int dbFormat;
#define GDI_FIXED_SPACE	0
#define GDI_DELIMITED	1

/*======= Error Severity Levels: dbErrLev ========*/
typedef int dbErrLev;
#define GDI_NOERROR 0
#define GDI_WARNING 2
#define GDI_FATAL 4

/*======= Database Options: dbOption and settings ===========*/
typedef int dbOption;
#define GDI_AUTO_COMMIT  1
#define GDI_PRO_C        2
#define GDI_VERSION      3
#define GDI_CONFIG       4    /* option to check if db is configured for gdi */
#define GDI_CHECK_OBJ    5    /* option to check for a user provided object  */
#define GDI_PROC_NAME    6    /* Pro*C connection "db" name                  */
#define GDI_VENDOR       7    /* database vendor for connection              */

/*======= settings for GDI_CONFIG dbOption =====*/
#define GDI_CONFIG_CHECK   "check"    /* return GDI_FAILURE if not configured */
#define GDI_CONFIG_INSTALL "install"  /* attempt to do config */
#define GDI_CONFIG_REMOVE  "remove"   /* remove gdi configuration from db */

/*======= Channels ========*/
#define GDI_NOT_USED	-1
#define GDI_NA		-1	/* not applicable */
#define GDI_NO_CHAN	-1
#define GDI_DEFAULT_CHAN 0
#define GDI_SELECT_CHAN  0
#define GDI_UPDATE_CHAN  1
#define GDI_INSERT_CHAN  1
#define GDI_CREATE_CHAN  1
#define GDI_DROP_CHAN    1

/*
 * forward decalaration of dbConn
 */
typedef struct conn_    dbConn;

/*
 * definition of dbObj
 */

typedef	struct
{
     char	name [GDI_MAX_NAME + 1];
     dbCtype	ctype;
     int	max_strlen;	/* max length if ctype is a string */
     int	max_arrlen;	/* max elements if ctype is an array */
     int	allow_null;
     int	dbtype;
     int	precision;	/* ORACLE only */
     int	scale;		/* ORACLE only */
     char	dbtype_s [GDI_MAX_TYPE + 1];
     char       utype_s  [GDI_MAX_TYPE + 1];  /* user defined db type, Sybase only */
} dbColDef;

/*======= coerce options: dbCoerce ========*/
typedef int dbCoerce;
#define GDI_DEFAULT_COERCE	0
#define GDI_DOUBLE_COERCE	1
#define GDI_STRING_COERCE	2
#define GDI_DBL_STR_COERCE	3

typedef	struct 
{
     Proto (int,	(*container_destroy),	(dbConn *, void *, void **));
     Proto (void *,	(*container_create),	(dbConn *, void **));
     Proto (int,	(*tuple_add),		(dbConn *, void **, void *, void **));
     Proto (void *,	(*tuple_retrieve),	(dbConn *, void *, int, void **));
     Proto (int,	(*tuple_destroy),	(dbConn *, void *, void **));
     Proto (void *,	(*tuple_create),	(dbConn *, dbColDef **, void **));
     Proto (int,	(*fill_data),		(dbConn *, int, dbColDef *, void *, 
						 void *, int, int, int, void **));
     Proto (void *,	(*get_data),		(dbConn *, int, dbColDef *, void *,
						 int *, int *, int *, void **));
     Proto (int, 	(*print_data),		(dbConn *, void *, dbFormat, int));
     void	*args;
     dbCoerce	coerce_option;
} dbConstr;

typedef struct
{
     int	rows_affected;
     char	cmd_num;
     int	more_rows;
     dbStatus	status;
} dbQuery;

typedef	struct dbobj_ 
{
     dbVendor	vendor_id;
     void	*tuples;
     int	n_tuples;
     dbColDef	**col_def;
     int	n_columns;
     char	*query;
     dbQuery    query_info;
     dbConstr	constructor;
     struct dbobj_	*next_obj;
     struct dbobj_	*prev_obj;
} dbObj;


/*
 * builtin constructors
 */

extern	dbConstr	GDI_DEFAULT;


/*
 * macros and prototypes for Access dbObj
 */

#define GDI_OBJ_TUPLES(dbobj)			((dbobj)->tuples)
#define GDI_OBJ_NUM_TUPLES(dbobj)		((dbobj)->n_tuples)
#define GDI_OBJ_NUM_COLUMNS(dbobj)		((dbobj)->n_columns)
#define GDI_OBJ_QUERY(dbobj)			((dbobj)->query)
#define GDI_OBJ_ROWS_AFFECTED(dbobj)		((dbobj)->query_info.rows_affected)
#define GDI_OBJ_CMD_NUM(dbobj)			((dbobj)->query_info.cmd_num)
#define GDI_OBJ_MORE_ROWS(dbobj)		((dbobj)->query_info.more_rows)
#define GDI_OBJ_STATUS(dbobj)			((dbobj)->query_info.status)
#define GDI_OBJ_CONSTRUCTOR(dbobj)		(&(dbobj)->constructor)
#define GDI_OBJ_COL_DEFS(dbobj)			((dbobj)->col_def)

#define	GDI_OBJ_COL_DEF(dbobj, col_num)		((dbobj)->col_def[(col_num)])
#define	GDI_OBJ_COL_NAME(dbobj, col_num)	((dbobj)->col_def[(col_num)]->name)
#define GDI_OBJ_COL_CTYPE(dbobj, col_num)	((dbobj)->col_def[(col_num)]->ctype)
#define GDI_OBJ_COL_LENGTH(dbobj, col_num)	((dbobj)->col_def[(col_num)]->max_strlen)
#define GDI_OBJ_COL_MAX_STRLEN(dbobj, col_num)	((dbobj)->col_def[(col_num)]->max_strlen)
#define GDI_OBJ_COL_MAX_ARRLEN(dbobj, col_num)  ((dbobj)->col_def[(col_num)]->max_arrlen)

#define GDI_OBJ_COL_ALLOW_NULL(dbobj, col_num)	((dbobj)->col_def[(col_num)]->allow_null)
#define GDI_OBJ_COL_PRECISION(dbobj, col_num)	((dbobj)->col_def[(col_num)]->precision)
#define GDI_OBJ_COL_SCALE(dbobj, col_num)	((dbobj)->col_def[(col_num)]->scale)
#define GDI_OBJ_COL_DBTYPE(dbobj, col_num)	((dbobj)->col_def[(col_num)]->dbtype)
#define GDI_OBJ_COL_DBTYPE_S(dbobj, col_num)    ((dbobj)->col_def[(col_num)]->dbtype_s)
#define GDI_OBJ_COL_UTYPE_S(dbobj, col_num)     ((dbobj)->col_def[(col_num)]->utype_s)

#define GDI_OBJ_COERCE_OPTION(dbobj)		(GDI_OBJ_CONSTRUCTOR(dbobj)->coerce_option)
#define GDI_OBJ_CONSTR_ARGS(dbobj)		(&(GDI_OBJ_CONSTRUCTOR(dbobj)->args))
#define GDI_OBJ_TUPLE_CONTAINER(dbobj)		(GDI_OBJ_TUPLES(dbobj))


Proto (int, gdi_obj_num_columns,
       (
	dbObj *dbobj		/* database object */
	));

Proto (int, gdi_obj_col_num,
       (
	dbObj *dbobj, 		/* database object */
	char  *col_name		/* column name	   */
	));

Proto (dbColDef*, gdi_obj_find_col_def,
       (
	dbObj *dbobj,		/* database object */
	char  *col_name		/* column name	   */
	));

Proto (void *, gdi_obj_find_value,
       (
	dbObj *dbobj,		/* database object */
	int   tuple_num,	/* tuple number    */
	char  *col_name		/* column name	   */
	));

Proto (dbObj *, gdi_obj_create, 
       (
	dbConstr *constr	/* database constructor */
	));

Proto (dbObj *, gdi_obj_destroy,
       (
	dbObj *dbobj		/* database object */
	));

Proto (void *, gdi_obj_value,
       (
	dbObj	*dbobj,		/* database object */
	int	tuple_num,	/* tuple number    */
	int	col_num		/* column number   */
	));
	

/*
 * prototypes for Access dbColDef
 */

Proto (dbColDef *, gdi_col_def_create,
       (
	char     *name, 	/* (i) column name           */
	dbCtype  ctype, 	/* (i) C type                */
	int      allow_null, 	/* (i) nulls are allowed in column            */
	int      max_strlen, 	/* (i) maximum string length (including NULL) */
	int      max_arrlen, 	/* (i) maximum array length  */
	int      dbtype,	/* (i) database type         */
	int      precision, 	/* (i) database precision    */
	int      scale, 	/* (i) database scale        */
	char     *dbtype_s	/* (i) database type string  */
	));

Proto (int, gdi_col_def_set_utype, 
       (
	dbColDef *coldef,	/* column definition */
	char     *utype         /* user type string  */
	));

Proto (int, gdi_col_def_destroy, 
       (
	dbColDef *coldef	/* column definition */
	));

Proto (int, gdi_obj_col_def_add, 
       (
	dbObj    *dbobj, 	/* database object   */
	dbColDef *coldef	/* column definition */
	));


/*
 * prototypes for accessing tuples through the dbConst
 */

Proto (int,	gdi_obj_container_create,
       (
	dbObj *dbobj		/* (i) database object */
	));

Proto (int,	gdi_obj_container_destroy,
       (
	dbObj *dbobj		/* (i) database object */
	));

Proto (void *,	gdi_obj_tuple_create,
       (
	dbObj *dbobj		/* (i) database object */
	));

Proto (int,	gdi_obj_tuple_destroy,
       (
	dbObj *dbobj,		/* (i) database object       */
	void  *tuple		/* (i) tuple to be destroyed */
	));

Proto (int,	gdi_obj_tuple_add,
       (
	dbObj *dbobj,		/* (i) database object   */
	void  *tuple		/* (i) tuple to be added */
	));

Proto (void *, gdi_obj_tuple_retrieve,
       (
	dbObj *dbobj,		/* (i) database object */
	int   tuple_num		/* (i) tuple number    */
	));

Proto (int, gdi_obj_fill_data,
       (
	dbObj *dbobj, 		/* (i) database object       */
	int   col_num,		/* (i) column number         */
	void  *tuple,		/* (i) tuple containing data */
	void  *dbdata,		/* (i) data to put in tuple  */
	int   is_null,		/* (i) is_null flag          */
	int   str_length,	/* (i) string length if data is string  */
	int   arr_length        /* (i) array length if data is an array */
	));		

Proto (void *, gdi_obj_get_data,
       (
	dbObj *dbobj, 		/* (i) database object       */
	int   col_num,		/* (i) column number         */
	void  *tuple,		/* (i) tuple containing data */
	int   *is_null,		/* (o) is_null flag          */
	int   *str_length,	/* (o) string length if data is string  */
	int   *arr_length       /* (o) array length if data is an array */
	));		


/*
 * prototypes for accessing tuples through the dbConst with error 
 * reporting on the dbconn
 */


Proto (int,	gdi_obj_container_create2,
       (
	dbConn *dbconn,         /* (i) database connector */
	dbObj  *dbobj		/* (i) database object    */
	));

Proto (int,	gdi_obj_container_destroy2,
       (
	dbConn *dbconn,         /* (i) database connector */
	dbObj  *dbobj		/* (i) database object    */
	));

Proto (void *,	gdi_obj_tuple_create2,
       (
	dbConn *dbconn,         /* (i) database connector */
	dbObj  *dbobj		/* (i) database object    */
	));

Proto (int,	gdi_obj_tuple_destroy2,
       (
	dbConn *dbconn,         /* (i) database connector    */
	dbObj  *dbobj,		/* (i) database object       */
	void   *tuple		/* (i) tuple to be destroyed */
	));

Proto (int,	gdi_obj_tuple_add2,
       (
	dbConn *dbconn,         /* (i) database connector */
	dbObj  *dbobj,		/* (i) database object    */
	void   *tuple		/* (i) tuple to be added  */
	));

Proto (void *, gdi_obj_tuple_retrieve2,
       (
	dbConn *dbconn,         /* (i) database connector */
	dbObj  *dbobj,		/* (i) database object    */
	int    tuple_num	/* (i) tuple number       */
	));

Proto (int, gdi_obj_fill_data2,
       (
	dbConn *dbconn,         /* (i) database connector    */
	dbObj  *dbobj, 		/* (i) database object       */
	int    col_num,		/* (i) column number         */
	void   *tuple,		/* (i) tuple containing data */
	void   *dbdata,		/* (i) data to put in tuple  */
	int    is_null,		/* (i) is_null flag          */
	int    str_length,	/* (i) string length if data is string  */
	int    arr_length       /* (i) array length if data is an array */
	));		

Proto (void *, gdi_obj_get_data2,
       (
	dbConn *dbconn,         /* (i) database connector    */
	dbObj  *dbobj, 		/* (i) database object       */
	int    col_num,		/* (i) column number         */
	void   *tuple,		/* (i) tuple containing data */
	int    *is_null,	/* (o) is_null flag          */
	int    *str_length,	/* (o) string length if data is string  */
	int    *arr_length      /* (o) array length if data is an array */
	));		


/*
 * definition of Default Tuple
 */

typedef	union 
{
     char	     char_val;
     int	     int_val;
     short	     short_val;
     int	     int_val;
     unsigned char   uchar_val;
     unsigned int    uint_val;
     unsigned short  ushort_val;
     unsigned int   uint_val;
     float	     float_val;
     double	     double_val;
     char	     *string;
     void	     *ptr;
} dbValue;

typedef	struct
{
     char	name [GDI_MAX_NAME + 1];
     char	dbtype_s [GDI_MAX_TYPE + 1];
     dbCtype	ctype;
     int	str_length;
     int	arr_length;
     int	is_null;
     dbValue	value;
} dbCol;

typedef	struct
{
     int	n_columns;
     dbCol	*col;
} dbTuple;


#define GDI_VALUE_INT(value)		((int)((value)->int_val))
#define GDI_VALUE_LONG(value)		((int)((value)->int_val))
#define GDI_VALUE_FLOAT(value)		((float)((value)->float_val))
#define GDI_VALUE_DOUBLE(value)		((double)((value)->double_val))
#define GDI_VALUE_CHAR(value)		((char)((value)->char_val))
#define GDI_VALUE_STRING(value)		((char*)((value)->string))
#define GDI_VALUE_PTR(value)		((void*)((value)->ptr))


#define GDI_ERROR_SIZE 	   255		/* size of the custom error string  */
#define GDI_CNAME_SIZE     10           /* max len of name for Sybase is 10 */
#define GDI_ACCOUNT_SIZE   30
#define GDI_DBNAME_SIZE    30
#define GDI_TABLENAME_SIZE 30
#define GDI_MACH_SIZE      30
#define GDI_NODE_SIZE      GDI_MACH_SIZE

#define GDI_SQLSTATE_SIZE  5            /* size of sql2 state string        */

/*
 * Database Connection
 */

typedef struct dbfuncs_ dbFuncs;

struct dbfuncs_
{
     Proto (int,   (*open),	       (dbConn *, char *, char *, char *, char *, 
					char *));
     Proto (int,   (*close),	       (dbConn *));
     Proto (int,   (*dead),            (dbConn *, int));
     Proto (int,   (*open_channel),    (dbConn *, int *));
     Proto (int,   (*close_channel),   (dbConn *, int));
     Proto (int,   (*channel_is_open), (dbConn *, int));
     Proto (void *,(*get_channel),     (dbConn *, int));
     Proto (int,   (*flush),           (dbConn *, int));
     Proto (int,   (*abort),           (dbConn *));
     Proto (int,   (*error_get),       (dbConn *, char *, int));
     Proto (int,   (*error_zero),      (dbConn *));
     Proto (int,   (*begin_tran),      (dbConn *, int, char *));
     Proto (int,   (*commit),          (dbConn *, int, char *));
     Proto (int,   (*rollback),        (dbConn *, int, char *));
     Proto (int,   (*savepoint),       (dbConn *, int, char *));
     Proto (int,   (*submit),          (dbConn *, char *, int, dbConstr *, dbObj **));
     Proto (int,   (*create_table),    (dbConn *, char *, dbObj *));
     Proto (int,   (*insert),          (dbConn *, char *, dbObj *));
     Proto (int,   (*get_counter),     (dbConn *, char *, char *, int, int *));
     Proto (int,   (*describe_object), (dbConn *, char *, dbConstr *, dbObj **));
     Proto (int,   (*what_is_object),  (dbConn *, char *, dbConstr *, dbObj **));
     Proto (int,   (*describe_query),  (dbConn *, char *, dbObj **));
     Proto (int,   (*temp_name),       (dbConn *, char *, char *));
     Proto (int,   (*set_date_mask),   (dbConn *, char *));
     Proto (int,   (*get_date_mask),   (dbConn *, char *, int));
     Proto (int,   (*trace),           (dbConn *, int, char *));
     Proto (int,   (*print_conn),      (dbConn *));
     Proto (int,   (*set_dboption),    (dbConn *, int, dbOption, char *));
     Proto (int,   (*get_dboption),    (dbConn *, int, dbOption, char *, int));
     Proto (int,   (*array_updel),     (dbConn *, char *, dbObj *));
     Proto (int,   (*dbinit),          (dbConn *));
};

struct conn_ 
{
     char       *home;                    /* value of environment variable   */
     char       version [GDI_VERSION_SIZE + 1];   /* library version   */
     int        descriptor;               /* dbConn descriptor               */
     char       name [GDI_CNAME_SIZE+1];  /* name of dbConn                  */
     dbVendor	vendor_id;
     char       vendor_s [GDI_VENDOR_SIZE+1];           /* vendor string     */
     void	*vendor_conn;             /* vendor specific connector       */
     void       *lib_handle;              /* dl handle for dynamic library   */
     char       account [GDI_ACCOUNT_SIZE+1];		/* user name         */
     char       database [GDI_DBNAME_SIZE+1];		/* database name     */
     char       machine [GDI_MACH_SIZE+1];		/* machine           */
     int	os_pid;			  /* operating system process id     */
     dbLang	lang;			  /* 'sql' or 'postquel' so far      */
     int        ansi_tm;                  /* ANSI trans mgt (TRUE or FALSE)  */
     dbStatus	status;
     dbErrLev	severity;
     char       sql_state [GDI_SQLSTATE_SIZE+1];        /* sql2 state        */
     int	error_code;               /* sql code or GDI error code      */
     char	error_text [GDI_ERROR_SIZE+1];
     dbDebug	debug;
     dbErrLev	threshold;
     void	(*error_handler) ();
     void	(*intr_handler) ();
     dbFuncs    *vendor_funcs;
     void       *user_data;               /* a place holder for anything the */
                                          /* user wants                      */
     dbConn	*prev_conn;
     dbConn	*next_conn;
};

/*
 * macros for accessing dbConn
 */

#define GDI_HOME(conn)                  ((conn)->home)
#define GDI_VERSION_CODE(conn)          ((conn)->version)
#define GDI_NAME(conn)                  ((conn)->name)
#define GDI_DESCRIPTOR(conn)            ((conn)->descriptor)
#define GDI_VENDOR_ID(conn)             ((conn)->vendor_id)
#define GDI_VENDOR_S(conn)              ((conn)->vendor_s)
#define GDI_LIB_HANDLE(conn)            ((conn)->lib_handle)
#define GDI_FUNCS(conn)                 ((conn)->vendor_funcs)
#define GDI_ACCOUNT(conn)		((conn)->account)
#define GDI_DATABASE(conn)		((conn)->database)
#define GDI_NODE(conn)			((conn)->machine)
#define GDI_OS_PID(conn)		((conn)->os_pid)
#define GDI_LANG(conn)			((conn)->lang)
#define GDI_ANSI_TM(conn)		((conn)->ansi_tm)

#define GDI_SQL_STATE(conn)             ((conn)->sql_state)
#define	GDI_ERROR_CODE(conn)		((conn)->error_code)
#define	GDI_ERROR_MSG(conn)		((conn)->error_text)
#define	GDI_ERROR_SEVERITY(conn)	((conn)->severity)
#define	GDI_ERROR_STATUS(conn)		((conn)->status)
#define GDI_ERROR_THRESHOLD(conn)	((conn)->threshold)
#define GDI_ERROR_DEBUG(conn)		((conn)->debug)


/*
 * prototypes for function to support linking of vendor specific libs
 */

Proto (int,      gdi_init,
       (
	char   *argv0,                  /* (i) name of executable */
	char   *gdihome                 /* (i) value of GDIHOME env variable */
	));

Proto (char **,  gdi_get_vendors,
       (
	void
	));

Proto (dbVendor, gdi_vendor_id, 
       (
	char *                          /* (i) vendor string */
	));

Proto (int,      gdi_link,
       (
	dbConn *conn			/* (i) database connector */
	));

Proto (int,      gdi_dbinit, 
       (
	dbConn *conn                    /* (i) database connector */
	));

/*
 * prototypes for Manage Database Connection functions
 */

Proto (dbConn *, gdi_open, 
       (
	char *vendor,			/* (i) database vendor  */
	char *account,			/* (i) database account */
	char *password,			/* (i) account password */
	char *database,			/* (i) database or machine */
	char *server,			/* (i) database server  */
	char *appname			/* (i) application name */
	));

Proto (dbConn *, gdi_open_oracle, 
       (
	char *account,			/* (i) database account */
	char *password,			/* (i) account password */
	char *database,			/* (i) database or machine */
	char *server,			/* (i) database server  */
	char *appname			/* (i) application name */
	));

Proto (dbConn *, gdi_open_proC, 
       (
	char *account,			/* (i) database account */
	char *password,			/* (i) account password */
	char *database,			/* (i) database or machine */
	char *server,			/* (i) database server  */
	char *appname			/* (i) application name */
	));

Proto (dbConn *, gdi_open_sybase, 
       (
	char *account,			/* (i) database account */
	char *password,			/* (i) account password */
	char *database,			/* (i) database or machine */
	char *server,			/* (i) database server  */
	char *appname			/* (i) application name */
	));

Proto (dbConn *, gdi_open_odbc, 
       (
	char *account,			/* (i) database account */
	char *password,			/* (i) account password */
	char *database,			/* (i) database or machine */
	char *server,			/* (i) database server  */
	char *appname			/* (i) application name */
	));

Proto (dbConn *, gdi_open_postgres, 
       (
	char *account,			/* (i) database account */
	char *password,			/* (i) account password */
	char *database,			/* (i) database or machine */
	char *server,			/* (i) database server  */
	char *appname			/* (i) application name */
	));

Proto (dbConn *, gdi_open_montage, 
       (
	char *account,			/* (i) database account */
	char *password,			/* (i) account password */
	char *database,			/* (i) database or machine */
	char *server,			/* (i) database server  */
	char *appname			/* (i) application name */
	));

Proto (int, gdi_close, 
       (
	dbConn *conn			/* (i) database connector */
	));

Proto (int, gdi_dead, 
       (
	dbConn *conn,			/* (i) database connector      */
	int    channo                   /* (i) database channel number */
	));

Proto (void, gdi_exit, (void));

Proto (dbConn *, gdi_get_conn, 
       (
	int    conn_desc                /* (i) conn descriptor */
	));

Proto (dbConn *, gdi_create_conn, 
       (
	char   *vendor,                 /* (i) database vendor string    */
	char   *conn_name               /* (i) connector name (optional) */
	));

Proto (int, gdi_free_conn,
       (
	dbConn *conn                    /* (i) database connector */
	));

/*
 * prototypes for Manage Database Channel functions
 */

Proto (int, gdi_open_channel,
       (
	dbConn	*conn,			/* (i) database connector */
	int     *channo			/* (o) channel number     */
	));

Proto (int, gdi_close_channel,
       (
	dbConn	*conn,			/* (i) database connector */
	int     channo			/* (i) channel number     */
	));

Proto (int, gdi_channel_is_open,
       (
	dbConn	*conn,			/* (i) database connector */
	int     channo			/* (i) channel number     */
	));

Proto (void *, gdi_get_channel,
       (
	dbConn	*conn,			/* (i) database connector */
	int     channo			/* (i) channel number     */
	));

Proto (int, gdi_flush,
       (
	dbConn	*conn,			/* (i) database connector */
	int     channo			/* (i) channel number     */
	));



/*
 * defines and prototypes Error Handler
 */

	/* 
	 * The errors defined here are GDI-specific errors and all 
	 * fall out of the range of UNIX ORACLE errors.  
	 */
/* compatible with old libdb30 */
#if 0
#define	BADDATA		10000	/* invalid data detected		*/
#define	UNIXERR		10001	/* a UNIX error occured in a db routine */
#define	WARNING		10002	/* a db warning (not used)		*/
#endif

/* new error codes */
#define GDI_BAD_CONFIG   9998	/* database is not configured */
#define GDI_DBERROR	 9999	/* for db that don't have int codes (Montage) */
#define	GDI_BADDATA	10000	/* invalid data detected		*/
#define	GDI_UNIXERR	10001	/* a UNIX error occured in a db routine */
#define	GDI_NODATA	10003	/* ??????? FATAL not found error   	*/
#define GDI_BADVENDOR	10004	/* unrecognized vendor id		*/
#define GDI_NOCONNECT	10005	/* not connected to a database		*/
#define GDI_DEADLOCK 	10006	/* deadlock detected                    */
#define GDI_TRUNCATION  10007   /* String truncated  */

Proto (int, gdi_error_init, 
       (
	dbConn   *conn,		/* (i) database connector */
	dbDebug  debug, 	/* (i) debug flag         */
	dbErrLev threshold,     /* (i) severity threshold */
	int      reserved1,	/* (i) reserved           */
	int      reserved2	/* (i) reserved           */
	));

Proto (int, gdi_error_zero, 
       (
	dbConn *conn		/* (i) database connector */
	));

Proto (int, gdi_error_get, 
       (
	dbConn   *conn,		/* (i) database connector */
	int      *errcode, 	/* (o) error code         */
	char     *errtext,	/* (o) error text         */
	int      maxtext, 	/* (i) max text length    */
	dbStatus *status,	/* (o) status	          */
	dbErrLev *severity	/* (o) severity           */
	));

Proto (int, gdi_error_flags, 
       (
	dbConn   *conn,		/* (i) database connector */
	dbDebug  *debug,	/* (o) debug flag	  */
	dbErrLev *threshold	/* (o) severity threshold */
	));

Proto (int, gdi_error_preface,
       (
	dbConn *conn,		/* (i) database connector */
	char   *errtext		/* (i) error text         */
	));

Proto (int, gdi_error_unix, 
       (
	dbConn *conn,		/* (i) database connector */
	char   *errtext		/* (i) error text         */
	));

Proto (int, gdi_error_app, 
       (
	dbConn *conn,		/* (i) database connector */
	int    errcode,		/* (i) error code         */
	char   *errtext  	/* (i) error text         */
	));

Proto (int, gdi_warning_app, 
       (
	dbConn *conn,		/* (i) database connector */
	int    errcode,		/* (i) error code         */
	char   *errtext		/* (i) error text         */
	));

Proto (int, gdi_inform, 
       (
	dbConn *conn,		/* (i) database connector */
	char   *fmt,            /* (i) format string      */
	...
	));


/*
 * prototypes for Transaction Manager functions
 */

Proto (int, gdi_commit, 
       (
	dbConn *conn,		/* (i) database connector  */
	int	channo,		/* (i) channel number      */
	char	*tran_name	/* (i) transaction name    */
	));

Proto (int, gdi_rollback,
       (
	dbConn  *conn,		/* (i) database connector  */
	int	channo,		/* (i) channel number      */
	char	*tran_name	/* (i) transaction name    */
	));

Proto (int, gdi_savepoint,
       (
	dbConn  *conn,		/* (i) database connector  */
	int	channo,		/* (i) channel number      */
	char	*save_name	/* (i) name of the savepoint */
	));

Proto (int, gdi_begin_tran, 
       (
	dbConn  *conn,		/* (i) database connector  */
	int	channo,		/* (i) channel number      */
	char	*tran_name	/* (i) transaction name    */
	));


/*
 * prototypes for Submit functions
 */
Proto (int, gdi_submit, 
       (
	dbConn   *conn,		/* (i) database connector  */
	char     *cmd_batch, 	/* (i) command batch       */
	int      max_records,   /* (i) maximum records     */
	dbConstr *constr,	/* (i) results constructor */
	dbObj    **results	/* (o) results             */
	));

Proto (int, gdi_abort, 
       (
	dbConn *conn		/* (i) database connector  */
	));


/*
 * prototypes for Create Table and Insert functions
 */
Proto (int, gdi_create_table, 
       (
	dbConn *conn,		/* (i) database connector */
	char   *table_name, 	/* (i) table name         */
	dbObj  *table_def	/* (i) table definition   */
	));

Proto (int, gdi_insert, 
       (
	dbConn *conn,		/* (i) database connector */
	char   *table_name, 	/* (i) table name         */
	dbObj  *datain		/* (i) data input         */
	));

Proto (int, gdi_array_updel, 
       (
	dbConn *conn,		/* (i) database connector */
	char   *query, 		/* (i) query with bind variables */
	dbObj  *datain		/* (i) data input         */
	));


/*
 * prototypes and macros for Temporary Table functions
 */

#define IS_TEMP(name)		((name) ? ((name)[0] == '#') : FALSE)

Proto (int, gdi_temp_name, 
       (
	dbConn *conn, 		/* (i) database connector */
	char   *name, 		/* (i) table name         */
	char   *new_name	/* (o) new table name     */ 
	));



/*
 * prototypes for Specialized Database functions 
 */

Proto (int, gdi_trace,
       (
	dbConn *conn,		/* (i) database connector   */
	int    mode,		/* (i) set trace TRUE/FALSE */
	char   *filename	/* (i) output file (Sybase only) */
	));

Proto (int, gdi_get_counter,
       (
	dbConn *conn,		/* (i) database connector  */
	char   *table_name,	/* (i) table name          */
	char   *ctr_name,	/* (i) counter name        */
	int    n_keys,		/* (i) number of keys      */
	int    *key_value	/* (o) key value           */
	));

Proto (int, gdi_describe_object,
       (
 	dbConn   *conn,		/* (i) database connector  */
	char     *object_name,	/* (i) object name         */
	dbConstr *constr,	/* (i) results constructor */
	dbObj    **results	/* (o) results             */
	));

Proto (int, gdi_what_is_object,
       (
 	dbConn   *conn,		/* (i) database connector  */
	char     *object_name,	/* (i) object name         */
	dbConstr *constr,	/* (i) results constructor */
	dbObj    **results	/* (o) results             */
	));

Proto (int, gdi_describe_query,
       (
 	dbConn   *conn,		/* (i) database connector  */
	char     *query,	/* (i) query               */
	dbObj    **results	/* (o) results             */
	));

	
Proto (int, gdi_get_account,
       (
 	dbConn	*conn,		/* (i) database connector  */
	char	*account,	/* (o) account		   */
	int	length		/* (i) account name length */
	));

	
Proto (int, gdi_get_database,
       (
 	dbConn  *conn,		/* (i) database connector   */
	char    *database,	/* (o) database		    */
	int	length		/* (i) database name length */
	));

	
Proto (int, gdi_get_node,
       (
 	dbConn  *conn,		/* (i) database connector   */
	char    *node,		/* (o) node		    */
	int	length		/* (i) node name length	    */
	));

	
Proto (void, gdi_sleep,
       (
 	dbConn  *conn,		/* (i) database connector  */
	int	max_sleep	/* (i) max sleep interval  */
	));



/*
 * set and get database options
 */
Proto (int, gdi_set_dboption,
       (
 	dbConn   *conn,		/* (i) database connector  */
	int      channo,        /* (i) channel number      */
	dbOption option,        /* (i) option to be set    */
	char     *setting       /* (i) value to set option to */
	));

Proto (int, gdi_get_dboption,
       (
 	dbConn   *conn,		/* (i) database connector  */
	int      channo,        /* (i) channel number      */
	dbOption option,        /* (i) option to be set    */
	char     *setting,      /* (o) value of option     */
	int      len            /* (i) length setting string */
	));


/*
 * prototypes for date mask functions
 */

#define GDI_DEFAULT_DATE_MASK   "19%02d%02d%02d %02d:%02d:%02d"
#define GDI_GMT_TIME            1
#define GDI_LOCAL_TIME          0

Proto (int, gdi_set_date_mask,
	(
	dbConn	*conn,		/* (i) database connector   */
	char	*new_mask	/* (i) new date mask string */
				/*     YYYYMMDD HH24:MI:SS  */
	));

Proto (int, gdi_get_date_mask,
	(
	dbConn	*conn,		/* (i) database connector   */
	char	*mask_holder,	/* (o) place to copy the date mask to */
	int	holder_size	/* (i) size of mask_holder  */
	));


Proto (int, gdi_get_date_now,
       (
	dbConn	*conn,		/* (i) database connector   */
	int     gmt_or_local,   /* (i) GMT or local flag    */
	char    *lddate         /* (o) date string          */
	));

Proto (int, gdi_compute_jdate,
       (
	double  tyme            /* (i) time in seconds since Jan 1, 1970 */
	));



/*
 * casts
 * user's beware:
 * floats and ints are promoted by the dbObj
 * No overflow checking is included here.
 */
#define GDI_CAST_VOID_STAR_2LONG(dbdata)	(* ((int *) dbdata))
#define GDI_CAST_VOID_STAR_2INT(dbdata)		(* ((int *) dbdata))
#define GDI_CAST_VOID_STAR_2DOUBLE(dbdata)	(* ((double *) dbdata))
#define GDI_CAST_VOID_STAR_2FLOAT(dbdata)	(* ((float *) dbdata))
#define GDI_CAST_VOID_STAR_2STR(dbdata)		(char *) dbdata
#define GDI_CAST_VOID_STAR_2CHAR(dbdata)	*((char *) dbdata)


/*
 * prototypes for Print functions
 */

Proto (int, gdi_print_conn,
       (
	dbConn	*conn		/* (i) database connector */
	));

Proto (int, gdi_print_dbobj,
       (
	dbObj	*obj		/* (i) database object	  */
	));

Proto (int, gdi_print_coldefs,
	(
	dbObj	 *dbobj	                /* (i) database object */
	));

Proto (int, gdi_print_tuple_header, 
       (
	dbObj    *dbobj, 		/* (i) database object              */
	dbFormat format 		/* (i) GDI_FIXED_SPACE or GDI_DELIMITED */
	));

Proto (int, gdi_print_tuples, /* print the actual data */
	(
	 dbObj    *dbobj, 		/* (i) database object              */
	 dbFormat format, 		/* (i) GDI_FIXED_SPACE or GDI_DELIMITED */
	 int	  header		/* (i) TRUE to print headings.  */
					/*     FALSE for data only      */
	));

/*
 * prototypes for FORTRAN interface functions
 */

Proto (int, gdi_cstr_to_for,
	(
	char	*cstr,		/* (i) C source string */
	int	c_length,	/* (i) length of the C string */
	char	*for_array,	/* (o) FORTRAN output array */
	int	f_length	/* (i) size of the FORTRAN array */
	));

Proto (int, gdi_for_to_cstr,
	(
	char	*cstr,		/* (o) C output string */
	int	c_length,	/* (i) length of the C string */
	char	*for_array,	/* (i) FORTRAN source array */
	int	f_length	/* (i) size of the FORTRAN array */
	));

#endif


