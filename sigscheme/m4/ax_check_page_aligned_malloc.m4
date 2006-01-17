dnl @synopsis AX_CHECK_PAGE_ALIGNED_MALLOC
dnl
dnl Some operating systems (generally, BSD Unix variants) lack a
dnl posix_memalign function, a memalign function, and a working
dnl (meaning, the memory can be freed) valloc function. To make up for
dnl it, the malloc function promises to return page-aligned addresses
dnl if more than one page's worth of memory is allocated.
dnl AX_CHECK_PAGE_ALIGNED_MALLOC checks for this condition and defines
dnl HAVE_PAGE_ALIGNED_MALLOC if the condition holds.
dnl
dnl As an aside, note that valloc'd memory cannot safely be freed on
dnl all operating systems. (Again, some flavors of BSD are the
dnl troublemakers.) It's best to avoid using valloc in favor of
dnl posix_memalign, memalign, or an aligned malloc as detected by
dnl AX_CHECK_PAGE_ALIGNED_MALLOC.
dnl
dnl Caveat: AX_CHECK_PAGE_ALIGNED_MALLOC takes a probabalistic
dnl approach. If 100 calls to malloc all return page-aligned addresses,
dnl it assumes that all calls will behave likewise. It is therefore
dnl possible -- albeit extremely unlikely -- that
dnl AX_CHECK_PAGE_ALIGNED_MALLOC can return a false positive.
dnl
dnl @category C
dnl @author Scott Pakin <pakin@uiuc.edu>
dnl @version 2005-01-22
dnl @license AllPermissive

AC_DEFUN([AX_CHECK_PAGE_ALIGNED_MALLOC],
[AC_CACHE_CHECK([if large mallocs guarantee page-alignment],
  [ax_cv_func_malloc_aligned],
  [AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif

int main()
{
  int pagesize = getpagesize();
  int i;

  for (i=0; i<100; i++)
    if ((unsigned long)malloc(pagesize+1) & (pagesize-1))
      exit (1);
  exit (0);
}
              ],
     [ax_cv_func_malloc_aligned=yes],
     [ax_cv_func_malloc_aligned=no],
     [ax_cv_func_malloc_aligned=no])
  ])
if test "$ax_cv_func_malloc_aligned" = yes ; then
  AC_DEFINE([HAVE_PAGE_ALIGNED_MALLOC], [1],
    [Define if `malloc'ing more than one page always returns a page-aligned address.])
fi
])
