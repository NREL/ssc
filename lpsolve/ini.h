/********************************************************************************************************************************************

Note
-------------
The version of lp_solve included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The lp_solve specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/lpsolve

The original version of lp_solve can be found at https://sourceforge.net/projects/lpsolve/

********************************************************************************************************************************************/
#include <stdio.h>

#ifdef __cplusplus
__EXTERN_C {
#endif

extern FILE *ini_create(char *filename);
extern FILE *ini_open(char *filename);
extern void ini_writecomment(FILE *fp, char *comment);
extern void ini_writeheader(FILE *fp, char *header, int addnewline);
extern void ini_writedata(FILE *fp, char *name, char *data);
extern int ini_readdata(FILE *fp, char *data, int szdata, int withcomment);
extern void ini_close(FILE *fp);

#ifdef __cplusplus
}
#endif
