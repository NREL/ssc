/********************************************************************************************************************************************

Note
-------------
The version of lp_solve included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The lp_solve specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/lpsolve

The original version of lp_solve can be found at https://sourceforge.net/projects/lpsolve/

********************************************************************************************************************************************/
#ifndef HEADER_lp_report
#define HEADER_lp_report

#ifdef __cplusplus
extern "C" {
#endif

/* General information functions */
char * __VACALL explain(lprec *lp, char *format, ...);
void __VACALL report(lprec *lp, int level, char *format, ...);

/* Prototypes for debugging and general data dumps */
void debug_print(lprec *lp, char *format, ...);
void debug_print_solution(lprec *lp);
void debug_print_bounds(lprec *lp, REAL *upbo, REAL *lowbo);
void blockWriteLREAL(FILE *output, char *label, LREAL *vector, int first, int last);
void blockWriteAMAT(FILE *output, const char *label, lprec* lp, int first, int last);
void blockWriteBMAT(FILE *output, const char *label, lprec* lp, int first, int last);


/* Model reporting headers */
void REPORT_objective(lprec *lp);
void REPORT_solution(lprec *lp, int columns);
void REPORT_constraints(lprec *lp, int columns);
void REPORT_duals(lprec *lp);
void REPORT_extended(lprec *lp);

/* Other rarely used, but sometimes extremely useful reports */
void REPORT_constraintinfo(lprec *lp, char *datainfo);
void REPORT_modelinfo(lprec *lp, MYBOOL doName, char *datainfo);
void REPORT_lp(lprec *lp);
MYBOOL REPORT_tableau(lprec *lp);
void REPORT_scales(lprec *lp);
MYBOOL REPORT_debugdump(lprec *lp, char *filename, MYBOOL livedata);
MYBOOL REPORT_mat_mmsave(lprec *lp, char *filename, int *colndx, MYBOOL includeOF, char *infotext);

#ifdef __cplusplus
 }
#endif

#endif /* HEADER_lp_report */

