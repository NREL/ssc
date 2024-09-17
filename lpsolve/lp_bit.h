/********************************************************************************************************************************************

Note
-------------
The version of lp_solve included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The lp_solve specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/lpsolve

The original version of lp_solve can be found at https://sourceforge.net/projects/lpsolve/

********************************************************************************************************************************************/
#include "lp_types.h"

#if defined INLINE
# define MYINLINE INLINE
#else
# define MYINLINE static
#endif

MYINLINE void set_biton(MYBOOL *bitarray, int item)
{
  bitarray[item / 8] |= (1 << (item % 8));
}

MYINLINE void set_bitoff(MYBOOL *bitarray, int item)
{
  bitarray[item / 8] &= ~(1 << (item % 8));
}

MYINLINE MYBOOL is_biton(MYBOOL *bitarray, int item)
{
  return( (MYBOOL) ((bitarray[item / 8] & (1 << (item % 8))) != 0) );
}
