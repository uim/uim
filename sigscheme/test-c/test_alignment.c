/*===========================================================================
 *  Filename : test_alignment.c
 *  About    : unit test for alignment on stack
 *
 *  Copyright (C) 2006 YamaKen <yamaken AT bp.iij4u.or.jp>
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the name of authors nor the names of its contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
 *  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 *  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 *  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
 *  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 *  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 *  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 *  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 *  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
===========================================================================*/

/* must be included prior to any SigScheme headers */
#include "cutter-sscm.h"

#include <stddef.h>
#include <sigscheme/sigscheme-stdint.h>
#include <sigscheme/sigscheme.h>

#define MSG(offs, exp) ("offset " #offs ": " #exp)

#define TEST_ALIGNMENT(od, offs)                                             \
    _UT_ASSERT_EQUAL_INT(0, (uintptr_t)&od.o##offs.d.i % sizeof(int), MSG(offs, int));                           \
    _UT_ASSERT_EQUAL_INT(0, (uintptr_t)&od.o##offs.d.l % sizeof(long), MSG(offs, long));                         \
    _UT_ASSERT_EQUAL_INT(0, (uintptr_t)&od.o##offs.d.p % sizeof(void *), MSG(offs, void *));                     \
    _UT_ASSERT_EQUAL_INT(0, (uintptr_t)&od.o##offs.d.cp.p % sizeof(void *), MSG(offs, {char; void *p;}.p));      \
    _UT_ASSERT_EQUAL_INT(0, (uintptr_t)&od.o##offs.d.sp.p % sizeof(void *), MSG(offs, {short; void *p;}.p));     \
    _UT_ASSERT_EQUAL_INT(0, (uintptr_t)&od.o##offs.d.c3p.p % sizeof(void *), MSG(offs, {char[3]; void *p;}.p));  \
    _UT_ASSERT_EQUAL_INT(0, (uintptr_t)&od.o##offs.d.ip.p % sizeof(void *), MSG(offs, {int; void *p;}.p));       \
    _UT_ASSERT_EQUAL_INT(0, (uintptr_t)&od.o##offs.d.c5p.p % sizeof(void *), MSG(offs, {char[5]; void *p;}.p));  \
    _UT_ASSERT_EQUAL_INT(0, (uintptr_t)&od.o##offs.d.lp.p % sizeof(void *), MSG(offs, {long; void *p;}.p));      \
    _UT_ASSERT_EQUAL_INT(0, (uintptr_t)&od.o##offs.d.qp.p % sizeof(void *), MSG(offs, {int64_t; void *p;}.p));   \
    _UT_ASSERT_EQUAL_INT(0, (uintptr_t)&od.o##offs.d.i32p.p % sizeof(void *), MSG(offs, {int32_t; void *p;}.p)); \
    _UT_ASSERT_EQUAL_INT(0, (uintptr_t)&od.o##offs.d.c9p.p % sizeof(void *), MSG(offs, {char[9]; void *p;}.p))


union data {
    short s;
    int i;
    long l;
    int64_t q;
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

    /* for LLP64 pointer alignment check */
    struct {
        int64_t q;
        void *p;
    } qp;

    /* for LP64 pointer alignment check */
    struct {
        int32_t i;
        void *p;
    } i32p;

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

UT_DEF2(test_1, "all")
{
    struct offsettable_data od;

    TEST_ALIGNMENT(od, 0);
    TEST_ALIGNMENT(od, 1);
    TEST_ALIGNMENT(od, 2);
    TEST_ALIGNMENT(od, 3);
    TEST_ALIGNMENT(od, 4);
    TEST_ALIGNMENT(od, 5);
    TEST_ALIGNMENT(od, 6);
    TEST_ALIGNMENT(od, 7);
}

UT_REGISTER_BEGIN("stack alignment")
UT_REGISTER(test_1, "all")
UT_REGISTER_END
