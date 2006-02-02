/*===========================================================================
 *  FileName : char.c
 *  About    : R5RS characters
 *
 *  Copyright (C) 2005-2006 Kazuki Ohta <mover AT hct.zaq.ne.jp>
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

#include "config.h"
#include "config-nonstd-string.h"

/*=======================================
  System Include
=======================================*/

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
/*
 * R5RS: 7.1.1 Lexical structure
 *
 * <token> --> <identifier> | <boolean> | <number> | <character> | <string>
 *      | ( | ) | #( | ' | ` | , | ,@ | .
 * <delimiter> --> <whitespace> | ( | ) | " | ;
 * <whitespace> --> <space or newline>
 * <comment> --> ;  <all subsequent characters up to a
 *                  line break>
 * <atmosphere> --> <whitespace> | <comment>
 * <intertoken space> --> <atmosphere>*
 * 
 * <identifier> --> <initial> <subsequent>* | <peculiar identifier>
 * <initial> --> <letter> | <special initial>
 * <letter> --> a | b | c | ... | z
 * 
 * <special initial> --> ! | $ | % | & | * | / | : | < | = | > | ? | ^ | _ | ~
 * <subsequent> --> <initial> | <digit> | <special subsequent>
 * <digit> --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
 * <special subsequent> --> + | - | . | @
 * <peculiar identifier> --> + | - | ...
 * <syntactic keyword> --> <expression keyword>
 *      | else | => | define 
 *      | unquote | unquote-splicing
 * <expression keyword> --> quote | lambda | if
 *      | set! | begin | cond | and | or | case
 *      | let | let* | letrec | do | delay
 *      | quasiquote
 * 
 * `<variable> => <'any <identifier> that isn't
 *                 also a <syntactic keyword>>
 * 
 * <boolean> --> #t | #f
 * <character> --> #\ <any character>
 *      | #\ <character name>
 * <character name> --> space | newline
 * 
 * <string> --> " <string element>* "
 * <string element> --> <any character other than " or \>
 *      | \" | \\ 
 * 
 * <number> --> <num 2>| <num 8>
 *      | <num 10>| <num 16>
 * 
 * 
 * <num R> --> <prefix R> <complex R>
 * <complex R> --> <real R> | <real R> @ <real R>
 *     | <real R> + <ureal R> i | <real R> - <ureal R> i
 *     | <real R> + i | <real R> - i
 *     | + <ureal R> i | - <ureal R> i | + i | - i
 * <real R> --> <sign> <ureal R>
 * <ureal R> --> <uinteger R>
 *     | <uinteger R> / <uinteger R>
 *     | <decimal R>
 * <decimal 10> --> <uinteger 10> <suffix>
 *     | . <digit 10>+ #* <suffix>
 *     | <digit 10>+ . <digit 10>* #* <suffix>
 *     | <digit 10>+ #+ . #* <suffix>
 * <uinteger R> --> <digit R>+ #*
 * <prefix R> --> <radix R> <exactness>
 *     | <exactness> <radix R>
 * 
 * <suffix> --> <empty> 
 *     | <exponent marker> <sign> <digit 10>+
 * <exponent marker> --> e | s | f | d | l
 * <sign> --> <empty>  | + |  -
 * <exactness> --> <empty> | #i | #e
 * <radix 2> --> #b
 * <radix 8> --> #o
 * <radix 10> --> <empty> | #d
 * <radix 16> --> #x
 * <digit 2> --> 0 | 1
 * <digit 8> --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
 * <digit 10> --> <digit>
 * <digit 16> --> <digit 10> | a | b | c | d | e | f 
 */

const unsigned char scm_char_class_table[] = {
    SCM_CH_CONTROL,            /*   0  nul       */
    SCM_CH_CONTROL,            /*   1  x01       */
    SCM_CH_CONTROL,            /*   2  x02       */
    SCM_CH_CONTROL,            /*   3  x03       */
    SCM_CH_CONTROL,            /*   4  x04       */
    SCM_CH_CONTROL,            /*   5  x05       */
    SCM_CH_CONTROL,            /*   6  x06       */
    SCM_CH_CONTROL,            /*   7  alarm     */
    SCM_CH_CONTROL,            /*   8  backspace */
    SCM_CH_CONTROL | SCM_CH_WHITESPACE, /*   9  tab       */
    SCM_CH_CONTROL | SCM_CH_WHITESPACE, /*  10  newline   */
    SCM_CH_CONTROL | SCM_CH_WHITESPACE, /*  11  vtab      */
    SCM_CH_CONTROL | SCM_CH_WHITESPACE, /*  12  page      */
    SCM_CH_CONTROL | SCM_CH_WHITESPACE, /*  13  return    */
    SCM_CH_CONTROL,            /*  14  x0e       */
    SCM_CH_CONTROL,            /*  15  x0f       */
    SCM_CH_CONTROL,            /*  16  x10       */
    SCM_CH_CONTROL,            /*  17  x11       */
    SCM_CH_CONTROL,            /*  18  x12       */
    SCM_CH_CONTROL,            /*  19  x13       */
    SCM_CH_CONTROL,            /*  20  x14       */
    SCM_CH_CONTROL,            /*  21  x15       */
    SCM_CH_CONTROL,            /*  22  x16       */
    SCM_CH_CONTROL,            /*  23  x17       */
    SCM_CH_CONTROL,            /*  24  x18       */
    SCM_CH_CONTROL,            /*  25  x19       */
    SCM_CH_CONTROL,            /*  26  x1a       */
    SCM_CH_CONTROL,            /*  27  esc       */
    SCM_CH_CONTROL,            /*  28  x1c       */
    SCM_CH_CONTROL,            /*  29  x1d       */
    SCM_CH_CONTROL,            /*  30  x1e       */
    SCM_CH_CONTROL,            /*  31  x1f       */
    SCM_CH_WHITESPACE,         /*  32  space     */
    SCM_CH_SPECIAL_INITIAL,    /*  33  !         */
    SCM_CH_TOKEN_INITIAL,      /*  34  "         */
    SCM_CH_TOKEN_INITIAL,      /*  35  #         */
    SCM_CH_SPECIAL_INITIAL,    /*  36  $         */
    SCM_CH_SPECIAL_INITIAL,    /*  37  %         */
    SCM_CH_SPECIAL_INITIAL,    /*  38  &         */
    SCM_CH_TOKEN_INITIAL,      /*  39  '         */
    SCM_CH_TOKEN_INITIAL,      /*  40  (         */
    SCM_CH_TOKEN_INITIAL,      /*  41  )         */
    SCM_CH_SPECIAL_INITIAL,    /*  42  *         */
    SCM_CH_SPECIAL_SUBSEQUENT, /*  43  +         */
    SCM_CH_TOKEN_INITIAL,      /*  44  ,         */
    SCM_CH_SPECIAL_SUBSEQUENT, /*  45  -         */
    SCM_CH_SPECIAL_SUBSEQUENT | SCM_CH_TOKEN_INITIAL, /*  46  .         */
    SCM_CH_SPECIAL_INITIAL,    /*  47  /         */
    SCM_CH_DIGIT,              /*  48  0         */
    SCM_CH_DIGIT,              /*  49  1         */
    SCM_CH_DIGIT,              /*  50  2         */
    SCM_CH_DIGIT,              /*  51  3         */
    SCM_CH_DIGIT,              /*  52  4         */
    SCM_CH_DIGIT,              /*  53  5         */
    SCM_CH_DIGIT,              /*  54  6         */
    SCM_CH_DIGIT,              /*  55  7         */
    SCM_CH_DIGIT,              /*  56  8         */
    SCM_CH_DIGIT,              /*  57  9         */
    SCM_CH_SPECIAL_INITIAL,    /*  58  :         */
    SCM_CH_TOKEN_INITIAL,      /*  59  ;         */
    SCM_CH_SPECIAL_INITIAL,    /*  60  <         */
    SCM_CH_SPECIAL_INITIAL,    /*  61  =         */
    SCM_CH_SPECIAL_INITIAL,    /*  62  >         */
    SCM_CH_SPECIAL_INITIAL,    /*  63  ?         */
    SCM_CH_SPECIAL_SUBSEQUENT, /*  64  @         */
    SCM_CH_HEX_LETTER,         /*  65  A         */
    SCM_CH_HEX_LETTER,         /*  66  B         */
    SCM_CH_HEX_LETTER,         /*  67  C         */
    SCM_CH_HEX_LETTER,         /*  68  D         */
    SCM_CH_HEX_LETTER,         /*  69  E         */
    SCM_CH_HEX_LETTER,         /*  70  F         */
    SCM_CH_NONHEX_LETTER,      /*  71  G         */
    SCM_CH_NONHEX_LETTER,      /*  72  H         */
    SCM_CH_NONHEX_LETTER,      /*  73  I         */
    SCM_CH_NONHEX_LETTER,      /*  74  J         */
    SCM_CH_NONHEX_LETTER,      /*  75  K         */
    SCM_CH_NONHEX_LETTER,      /*  76  L         */
    SCM_CH_NONHEX_LETTER,      /*  77  M         */
    SCM_CH_NONHEX_LETTER,      /*  78  N         */
    SCM_CH_NONHEX_LETTER,      /*  79  O         */
    SCM_CH_NONHEX_LETTER,      /*  80  P         */
    SCM_CH_NONHEX_LETTER,      /*  81  Q         */
    SCM_CH_NONHEX_LETTER,      /*  82  R         */
    SCM_CH_NONHEX_LETTER,      /*  83  S         */
    SCM_CH_NONHEX_LETTER,      /*  84  T         */
    SCM_CH_NONHEX_LETTER,      /*  85  U         */
    SCM_CH_NONHEX_LETTER,      /*  86  V         */
    SCM_CH_NONHEX_LETTER,      /*  87  W         */
    SCM_CH_NONHEX_LETTER,      /*  88  X         */
    SCM_CH_NONHEX_LETTER,      /*  89  Y         */
    SCM_CH_NONHEX_LETTER,      /*  90  Z         */
    SCM_CH_TOKEN_INITIAL,      /*  91  [         */
    SCM_CH_CONTROL,            /*  92  \\        */
    SCM_CH_TOKEN_INITIAL,      /*  93  ]         */
    SCM_CH_SPECIAL_INITIAL,    /*  94  ^         */
    SCM_CH_SPECIAL_INITIAL,    /*  95  _         */
    SCM_CH_TOKEN_INITIAL,      /*  96  `         */
    SCM_CH_HEX_LETTER,         /*  97  a         */
    SCM_CH_HEX_LETTER,         /*  98  b         */
    SCM_CH_HEX_LETTER,         /*  99  c         */
    SCM_CH_HEX_LETTER,         /* 100  d         */
    SCM_CH_HEX_LETTER,         /* 101  e         */
    SCM_CH_HEX_LETTER,         /* 102  f         */
    SCM_CH_NONHEX_LETTER,      /* 103  g         */
    SCM_CH_NONHEX_LETTER,      /* 104  h         */
    SCM_CH_NONHEX_LETTER,      /* 105  i         */
    SCM_CH_NONHEX_LETTER,      /* 106  j         */
    SCM_CH_NONHEX_LETTER,      /* 107  k         */
    SCM_CH_NONHEX_LETTER,      /* 108  l         */
    SCM_CH_NONHEX_LETTER,      /* 109  m         */
    SCM_CH_NONHEX_LETTER,      /* 110  n         */
    SCM_CH_NONHEX_LETTER,      /* 111  o         */
    SCM_CH_NONHEX_LETTER,      /* 112  p         */
    SCM_CH_NONHEX_LETTER,      /* 113  q         */
    SCM_CH_NONHEX_LETTER,      /* 114  r         */
    SCM_CH_NONHEX_LETTER,      /* 115  s         */
    SCM_CH_NONHEX_LETTER,      /* 116  t         */
    SCM_CH_NONHEX_LETTER,      /* 117  u         */
    SCM_CH_NONHEX_LETTER,      /* 118  v         */
    SCM_CH_NONHEX_LETTER,      /* 119  w         */
    SCM_CH_NONHEX_LETTER,      /* 120  x         */
    SCM_CH_NONHEX_LETTER,      /* 121  y         */
    SCM_CH_NONHEX_LETTER,      /* 122  z         */
    SCM_CH_TOKEN_INITIAL,      /* 123  {         */
    SCM_CH_TOKEN_INITIAL,      /* 124  |         */
    SCM_CH_TOKEN_INITIAL,      /* 125  }         */
    SCM_CH_SPECIAL_INITIAL,    /* 126  ~         */
    SCM_CH_CONTROL,            /* 127  delete    */
};

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Implementations
=======================================*/
/*===========================================================================
  R5RS : 6.3 Other data types : 6.3.4 Characters
===========================================================================*/
ScmObj
scm_p_charp(ScmObj obj)
{
    DECLARE_FUNCTION("char?", procedure_fixed_1);

    return MAKE_BOOL(CHARP(obj));
}

ScmObj
scm_p_char_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char=?", procedure_fixed_2);

    ENSURE_CHAR(ch1);
    ENSURE_CHAR(ch2);

#if SCM_HAS_IMMEDIATE_CHAR_ONLY
    return MAKE_BOOL(EQ(ch1, ch2));
#else
    return MAKE_BOOL(SCM_CHAR_VALUE(ch1) == SCM_CHAR_VALUE(ch2));
#endif
}

#define CHAR_CMP_BODY(op, ch1, ch2)                                          \
    do {                                                                     \
        ENSURE_CHAR(ch1);                                                    \
        ENSURE_CHAR(ch2);                                                    \
                                                                             \
        return MAKE_BOOL(SCM_CHAR_VALUE(ch1) op SCM_CHAR_VALUE(ch2));        \
    } while (/* CONSTCOND */ 0)

ScmObj
scm_p_char_lessp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char<?", procedure_fixed_2);

    CHAR_CMP_BODY(<, ch1, ch2);
}

ScmObj
scm_p_char_greaterp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char>?", procedure_fixed_2);

    CHAR_CMP_BODY(>, ch1, ch2);
}

ScmObj
scm_p_char_less_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char<=?", procedure_fixed_2);

    CHAR_CMP_BODY(<=, ch1, ch2);
}

ScmObj
scm_p_char_greater_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char>=?", procedure_fixed_2);

    CHAR_CMP_BODY(>=, ch1, ch2);
}

#define CHAR_CI_CMP_BODY(op, ch1, ch2)                                       \
    do {                                                                     \
        scm_ichar_t val1, val2;                                              \
                                                                             \
        ENSURE_CHAR(ch1);                                                    \
        ENSURE_CHAR(ch2);                                                    \
                                                                             \
        val1 = ICHAR_FOLDCASE(SCM_CHAR_VALUE(ch1));                          \
        val2 = ICHAR_FOLDCASE(SCM_CHAR_VALUE(ch2));                          \
                                                                             \
        return MAKE_BOOL(val1 op val2);                                      \
    } while (/* CONSTCOND */ 0)

ScmObj
scm_p_char_ci_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci=?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(==, ch1, ch2);
}

ScmObj
scm_p_char_ci_lessp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci<?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(<, ch1, ch2);
}

ScmObj
scm_p_char_ci_greaterp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci>?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(>, ch1, ch2);
}

ScmObj
scm_p_char_ci_less_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci<=?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(<=, ch1, ch2);
}

ScmObj
scm_p_char_ci_greater_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci>=?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(>=, ch1, ch2);
}

#undef CHAR_CMP_BODY
#undef CHAR_CI_CMP_BODY

ScmObj
scm_p_char_alphabeticp(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-alphabetic?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(ICHAR_ALPHABETICP(val));
}

ScmObj
scm_p_char_numericp(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-numeric?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(ICHAR_NUMERICP(val));
}

ScmObj
scm_p_char_whitespacep(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-whitespace?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(ICHAR_WHITESPACEP(val));
}

ScmObj
scm_p_char_upper_casep(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-upper-case?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(ICHAR_UPPER_CASEP(val));
}

ScmObj
scm_p_char_lower_casep(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-lower-case?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(ICHAR_LOWER_CASEP(val));
}

ScmObj
scm_p_char2integer(ScmObj ch)
{
    DECLARE_FUNCTION("char->integer", procedure_fixed_1);

    ENSURE_CHAR(ch);

    return MAKE_INT(SCM_CHAR_VALUE(ch));
}

ScmObj
scm_p_integer2char(ScmObj n)
{
    scm_int_t val;
    DECLARE_FUNCTION("integer->char", procedure_fixed_1);

    ENSURE_INT(n);

    val = SCM_INT_VALUE(n);
#if SCM_USE_MULTIBYTE_CHAR
    if (!SCM_CHARCODEC_CHAR_LEN(scm_current_char_codec, val))
#else
    if (!ICHAR_ASCIIP(val))
#endif
        ERR_OBJ("invalid char value", n);

    return MAKE_CHAR(val);
}

ScmObj
scm_p_char_upcase(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-upcase", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);
    SCM_CHAR_SET_VALUE(ch, ICHAR_UPCASE(val));

    return ch;
}

ScmObj
scm_p_char_downcase(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-downcase", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);
    SCM_CHAR_SET_VALUE(ch, ICHAR_DOWNCASE(val));

    return ch;
}
