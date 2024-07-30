/********************************************************************************************************************************************

Note
-------------
The version of lp_solve included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The lp_solve specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/lpsolve

The original version of lp_solve can be found at https://sourceforge.net/projects/lpsolve/

********************************************************************************************************************************************/
#include "lp_lib.h"
#include "lp_report.h"

#define MALLOC(ptr, nr, type)\
  ((((nr) == 0) || ((ptr = (type *) malloc((size_t)((nr) * sizeof(*ptr)))) == NULL)) ? \
   report(NULL, CRITICAL, "malloc of %d bytes failed on line %d of file %s\n",\
           (nr) * sizeof(*ptr), __LINE__, __FILE__), (ptr = NULL /* (void *) 0 */) : \
   ptr\
  )

#define CALLOC(ptr, nr, type)\
  ((((nr) == 0) || ((ptr = (type *) calloc((size_t)(nr), sizeof(*ptr))) == NULL)) ? \
   report(NULL, CRITICAL, "calloc of %d bytes failed on line %d of file %s\n",\
           (nr) * sizeof(*ptr), __LINE__, __FILE__), (ptr = NULL /* (void *) 0 */) : \
   ptr\
  )

#define REALLOC(ptr, nr, type)\
  ((((nr) == 0) || ((ptr = (type *) realloc(ptr, (size_t)((nr) * sizeof(*ptr)))) == NULL)) ? \
   report(NULL, CRITICAL, "realloc of %d bytes failed on line %d of file %s\n",\
           (nr) * sizeof(*ptr), __LINE__, __FILE__), (ptr = NULL /* (void *) 0 */) : \
   ptr\
  )

#if defined FREE
# undef FREE
#endif

#define FREE(ptr) if (ptr != NULL) {free(ptr), ptr = NULL;} else

#define MALLOCCPY(nptr, optr, nr, type)\
  (MALLOC(nptr, nr, type), (nptr != NULL) ? memcpy(nptr, optr, (size_t)((nr) * sizeof(*optr))) : 0, nptr)
