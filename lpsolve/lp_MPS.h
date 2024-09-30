/********************************************************************************************************************************************

Note
-------------
The version of lp_solve included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The lp_solve specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/lpsolve

The original version of lp_solve can be found at https://sourceforge.net/projects/lpsolve/

********************************************************************************************************************************************/
#ifndef HEADER_lp_MPS
#define HEADER_lp_MPS

#include "lp_types.h"

/* For MPS file reading and writing */
#define ROWNAMEMASK          "R%d"
#define ROWNAMEMASK2         "r%d"
#define COLNAMEMASK          "C%d"
#define COLNAMEMASK2         "c%d"

#ifdef __cplusplus
extern "C" {
#endif

/* Read an MPS file */
MYBOOL MPS_readfile(lprec **newlp, char *filename, int typeMPS, int verbose);
MYBOOL __WINAPI MPS_readhandle(lprec **newlp, FILE *filehandle, int typeMPS, int verbose);

/* Write a MPS file to output */
MYBOOL MPS_writefile(lprec *lp, int typeMPS, char *filename);
MYBOOL MPS_writehandle(lprec *lp, int typeMPS, FILE *output);

/* Read and write BAS files */
MYBOOL MPS_readBAS(lprec *lp, int typeMPS, char *filename, char *info);
MYBOOL MPS_writeBAS(lprec *lp, int typeMPS, char *filename);

#ifdef __cplusplus
 }
#endif

#endif /* HEADER_lp_MPS */

