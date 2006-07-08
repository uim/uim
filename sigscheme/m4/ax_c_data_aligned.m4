dnl @synopsis AX_C_DATA_ALIGNED
dnl
dnl Checks whether data types are properly aligned.
dnl
dnl @category C
dnl @author YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
dnl @version 2006-06-04
dnl @license AllPermissive

AC_DEFUN([AX_C_DATA_ALIGNED], [
  AC_CACHE_CHECK([whether data types are properly aligned],
    ax_cv_c_data_aligned,
    [AC_TRY_RUN([
#include <stdlib.h>
#include <stdio.h>

#define DEBUG_PRINT 0

#define MY_ASSERT(cond) if (!(cond)) return EXIT_FAILURE

#define TEST_ALIGNMENT(od, offs)                                             \
    MY_ASSERT(!((unsigned long)&od.o##offs.d.i      % sizeof(int)));         \
    MY_ASSERT(!((unsigned long)&od.o##offs.d.l      % sizeof(long)));        \
    MY_ASSERT(!((unsigned long)&od.o##offs.d.p      % sizeof(void *)));      \
    MY_ASSERT(!((unsigned long)&od.o##offs.d.cp.p   % sizeof(void *)));      \
    MY_ASSERT(!((unsigned long)&od.o##offs.d.sp.p   % sizeof(void *)));      \
    MY_ASSERT(!((unsigned long)&od.o##offs.d.c3p.p  % sizeof(void *)));      \
    MY_ASSERT(!((unsigned long)&od.o##offs.d.ip.p   % sizeof(void *)));      \
    MY_ASSERT(!((unsigned long)&od.o##offs.d.c5p.p  % sizeof(void *)));      \
    MY_ASSERT(!((unsigned long)&od.o##offs.d.lp.p   % sizeof(void *)));      \
    MY_ASSERT(!((unsigned long)&od.o##offs.d.c9p.p  % sizeof(void *)))

union data {
  short s;
  int i;
  long l;
  void *p;

  struct {
      char c;
      void *p;
  } cp;

  struct {
      short s;
      void *p;
  } sp;

  struct {
      char c[3];
      void *p;
  } c3p;

  struct {
      int i;
      void *p;
  } ip;

  struct {
      char c[5];
      void *p;
  } c5p;

  struct {
      long l;
      void *p;
  } lp;

  struct {
      char c[9];
      void *p;
  } c9p;
};

struct offsettable_data {
  struct {
      union data d;
  } o0;

  struct {
      char offset[1];
      union data d;
  } o1;

  struct {
      char offset[2];
      union data d;
  } o2;

  struct {
      char offset[3];
      union data d;
  } o3;

  struct {
      char offset[4];
      union data d;
  } o4;

  struct {
      char offset[5];
      union data d;
  } o5;

  struct {
      char offset[6];
      union data d;
  } o6;

  struct {
      char offset[7];
      union data d;
  } o7;
};

int
long_aligned_pad0(void)
{
  long l0;
  long l1;

#if DEBUG_PRINT
  /* l0 = 0xafaddca4, l1 = 0xafaddca0 */
  /* the long variables are always aligned on -O0, -O1, -O2, -O3 and -Os */
  printf("l0 = %p, l1 = %p\n", &l0, &l1);
#endif

  return !((size_t)&l0 % sizeof(long) || (size_t)&l1 % sizeof(long));
}

int
long_aligned_pad1(void)
{
  long l0;
  char c;
  long l1;

#if DEBUG_PRINT
  /* gcc 4.1 on i386 reorders variables if -O2 */
  /* the long variables are always aligned on -O0, -O1, -O2, -O3 and -Os */
  /* l0 = 0xafaddca0, c = 0xafaddca7, l1 = 0xafaddc9c */
  printf("l0 = %p, c = %p, l1 = %p\n", &l0, &c, &l1);
#endif

  return !((size_t)&l0 % sizeof(long) || (size_t)&l1 % sizeof(long));
}

int
long_aligned_pad2(void)
{
  long l0;
  char c[2];
  long l1;

#if DEBUG_PRINT
  /* gcc 4.1 on i386 reorders variables if -O2 */
  /* and the long variables are always aligned on -O0, -O1, -O2, -O3 and -Os */
  /* l0 = 0xafaddca0, c = 0xafaddca6, l1 = 0xafaddc9c */
  printf("l0 = %p, c = %p, l1 = %p\n", &l0, &c, &l1);
#endif

  return !((size_t)&l0 % sizeof(long) || (size_t)&l1 % sizeof(long));
}

int
long_aligned_pad3(void)
{
  long l0;
  char c[3];
  long l1;

#if DEBUG_PRINT
  /* gcc 4.1 on i386 reorders variables if -O2 */
  /* and the long variables are always aligned on -O0, -O1, -O2, -O3 and -Os */
  /* l0 = 0xafaddca0, c = 0xafaddca5, l1 = 0xafaddc9c */
  printf("l0 = %p, c = %p, l1 = %p\n", &l0, &c, &l1);
#endif

  return !((size_t)&l0 % sizeof(long) || (size_t)&l1 % sizeof(long));
}

int
long_aligned_pad4(void)
{
  long l0;
  char c[4];
  long l1;

#if DEBUG_PRINT
  /* gcc 4.1 on i386: specified order even if -O2 if sizes do not differ */
  /* and the long variables are always aligned on -O0, -O1, -O2, -O3 and -Os */
  /* l0 = 0xafaddca4, c = 0xafaddca0, l1 = 0xafaddc9c */
  printf("l0 = %p, c = %p, l1 = %p\n", &l0, &c, &l1);
#endif

  return !((size_t)&l0 % sizeof(long) || (size_t)&l1 % sizeof(long));
}

int
main(int argc, char **argv)
{
  struct offsettable_data od;
  int ok;

  TEST_ALIGNMENT(od, 0);
  TEST_ALIGNMENT(od, 1);
  TEST_ALIGNMENT(od, 2);
  TEST_ALIGNMENT(od, 3);
  TEST_ALIGNMENT(od, 4);
  TEST_ALIGNMENT(od, 5);
  TEST_ALIGNMENT(od, 6);
  TEST_ALIGNMENT(od, 7);

  ok = long_aligned_pad0();
  if (!ok) return EXIT_FAILURE;

  ok = long_aligned_pad1();
  if (!ok) return EXIT_FAILURE;

  ok = long_aligned_pad2();
  if (!ok) return EXIT_FAILURE;

  ok = long_aligned_pad3();
  if (!ok) return EXIT_FAILURE;

  ok = long_aligned_pad4();
  if (!ok) return EXIT_FAILURE;

  return EXIT_SUCCESS;
}
    ],
    ax_cv_c_data_aligned=yes,
    ax_cv_c_data_aligned=no,
    ax_cv_c_data_aligned=yes)
  ])
])
