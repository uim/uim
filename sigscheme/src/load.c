/*===========================================================================
 *  FileName : load.c
 *  About    : Code loading
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

/*=======================================
  System Include
=======================================*/
#include <stddef.h>
#include <stdio.h>
#include <string.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"
#if SCM_USE_MULTIBYTE_CHAR
#include "mbcport.h"
#endif

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/
#if SCM_USE_SRFI22
/* SRFI-22: The <script prelude> line may not be longer than 64 characters. */
#define SCRIPT_PRELUDE_MAXLEN 64
#define SCRIPT_PRELUDE_DELIM  " \t\n\r"
#endif

/*=======================================
  Variable Declarations
=======================================*/
const char *scm_lib_path = NULL;

/*=======================================
  File Local Function Declarations
=======================================*/
static void scm_load_internal(const char *filename);
static char *find_path(const char *c_filename);
static scm_bool file_existsp(const char *filepath);
#if SCM_USE_SRFI22
static void interpret_script_prelude(ScmObj port);
static char **parse_script_prelude(ScmObj port);
#endif

/*=======================================
  Function Implementations
=======================================*/
void
scm_set_lib_path(const char *path)
{
    scm_lib_path = path;
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.4 System Interface
===========================================================================*/
void
scm_load(const char *filename)
{
#if !SCM_GCC4_READY_GC
    ScmObj stack_start;
#endif

#if SCM_GCC4_READY_GC
    SCM_GC_PROTECTED_CALL_VOID(scm_load_internal, (filename));
#else
    scm_gc_protect_stack(&stack_start);

    scm_load_internal(filename);

    scm_gc_unprotect_stack(&stack_start);
#endif
}

static void
scm_load_internal(const char *filename)
{
    ScmObj path, port, sexp;
    char *c_path;
#if SCM_USE_MULTIBYTE_CHAR
    ScmCharCodec *saved_codec;
#endif
    DECLARE_INTERNAL_FUNCTION("load");

    CDBG((SCM_DBG_FILE, "loading ~S", filename));

    c_path = find_path(filename);
    if (!c_path)
        ERR("file \"~S\" not found", filename);

    path = MAKE_IMMUTABLE_STRING(c_path, STRLEN_UNKNOWN);
    port = scm_p_open_input_file(path);

#if SCM_USE_MULTIBYTE_CHAR
    saved_codec = scm_current_char_codec;
#endif
#if SCM_USE_SRFI22
    if (scm_port_peek_char(port) == '#')
        interpret_script_prelude(port);
#endif

    /* read & eval cycle */
    while (sexp = scm_read(port), !EOFP(sexp))
        EVAL(sexp, SCM_INTERACTION_ENV);

    scm_p_close_input_port(port);
#if SCM_USE_MULTIBYTE_CHAR
    scm_current_char_codec = saved_codec;
#endif

    CDBG((SCM_DBG_FILE, "done."));
}

/* FIXME: reject relative paths to ensure security */
static char *
find_path(const char *filename)
{
    char *path;
    size_t lib_path_len, filename_len, path_len;

    SCM_ASSERT(filename);

    /* try absolute and relative path */
    if (file_existsp(filename))
        return scm_strdup(filename);

    /* try under scm_lib_path */
    if (scm_lib_path) {
        lib_path_len = scm_lib_path ? strlen(scm_lib_path) : 0;
        filename_len = strlen(filename);
        path_len = lib_path_len + sizeof((char)'/') + filename_len + sizeof("");

        path = scm_malloc(path_len);
        snprintf(path, path_len, "%s/%s", scm_lib_path, filename);
        if (file_existsp(path))
            return path;
        free(path);
    }

    return NULL;
}

static scm_bool
file_existsp(const char *c_filepath)
{
    FILE *f;

    f = fopen(c_filepath, "r");
    if (f) {
        fclose(f);
        return scm_true;
    } else {
        return scm_false;
    }
}

ScmObj
scm_p_load(ScmObj filename)
{
    DECLARE_FUNCTION("load", procedure_fixed_1);

    ENSURE_STRING(filename);

    scm_load_internal(SCM_STRING_STR(filename));

    return SCM_UNDEF;
}

#if SCM_USE_SRFI22
static void
interpret_script_prelude(ScmObj port)
{
    char **argv;

    argv = parse_script_prelude(port);
    scm_interpret_argv(argv);
#if SCM_USE_MULTIBYTE_CHAR
    if (SCM_CHARPORT_DYNAMIC_CAST(ScmMultiByteCharPort, SCM_PORT_IMPL(port))) {
        ScmMultiByteCharPort_set_codec(SCM_PORT_IMPL(port),
                                       scm_current_char_codec);
    }
#endif
    scm_free_argv(argv);
}

static char **
parse_script_prelude(ScmObj port)
{
    scm_ichar_t c;
    int argc, len, line_len;
    char **argv, *arg, *p;
    char line[SCRIPT_PRELUDE_MAXLEN];

    for (p = line; p < &line[SCRIPT_PRELUDE_MAXLEN]; p++) {
        c = scm_port_get_char(port);
        if (!ICHAR_ASCIIP(c))
            PLAIN_ERR("non-ASCII char appeared in UNIX script prelude");
        if (c == SCM_NEWLINE_STR[0]) {
            *p = '\0';
            break;
        }
        *p = c;
    }
    if (*p)
        PLAIN_ERR("too long UNIX script prelude (max 64)");
    line_len = p - line;

    if (line[0] != '#' || line[1] != '!') {
        PLAIN_ERR("Invalid UNIX script prelude");
    }
#if 1
    /* strict check */
    if (line[2] != ' ') {
        PLAIN_ERR("Invalid UNIX script prelude: "
                  "SRFI-22 requires a space after hash-bang sequence");
    }
#endif

    argv = scm_malloc(sizeof(char *));
    argv[0] = NULL;
    argc = 0;
    for (p = &line[3]; p < &line[line_len]; p += len + 1) {
        p += strspn(p, SCRIPT_PRELUDE_DELIM);
        len = strcspn(p, SCRIPT_PRELUDE_DELIM);
        if (!len)
            break;
        p[len] = '\0';
        arg = scm_strdup(p);
        argv[argc] = arg;
        argv = scm_realloc(argv, sizeof(char *) * (++argc + 1));
        argv[argc] = NULL;
    }

    return argv;
}
#endif /* SCM_USE_SRFI22 */
