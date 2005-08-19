/*===========================================================================
 *  FileName : io.c
 *  About    : io related functions
 *
 *  Copyright (C) 2005      by Kazuki Ohta (mover@hct.zaq.ne.jp)
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
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 *  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
===========================================================================*/
/*=======================================
  System Include
=======================================*/
#include <stdio.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
ScmObj current_input_port   = NULL;
ScmObj current_output_port  = NULL;

ScmObj SigScm_features      = NULL;

static const char *lib_path = NULL;

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj create_loaded_str(ScmObj filename);

/*=======================================
  Function Implementations
=======================================*/
void SigScm_set_lib_path(const char *path)
{
    lib_path = path;
}

/*=======================================
  R5RS : 6.6 Input and Output
=======================================*/
/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.1 Ports
===========================================================================*/
ScmObj ScmOp_call_with_input_file(ScmObj filepath, ScmObj proc)
{
    ScmObj port = SCM_NIL;
    ScmObj ret  = SCM_NIL;

    if (!SCM_STRINGP(filepath))
        SigScm_ErrorObj("call-with-input-file : string required but got", filepath);
    if (!SCM_FUNCP(proc) && !SCM_CLOSUREP(proc))
        SigScm_ErrorObj("call-with-input-file : proc required but got ", proc);
    
    /* open port */
    port = ScmOp_open_input_file(filepath);
    
    /* (apply proc (port)) */
    ret = ScmOp_apply(Scm_NewCons(proc,
                                  Scm_NewCons(Scm_NewCons(port, SCM_NIL),
                                              SCM_NIL)),
                      SCM_NIL);

    /* close port */
    ScmOp_close_input_port(port);

    return ret;
}

ScmObj ScmOp_call_with_output_file(ScmObj filepath, ScmObj proc)
{
    ScmObj port = SCM_NIL;
    ScmObj ret  = SCM_NIL;

    if (!SCM_STRINGP(filepath))
        SigScm_ErrorObj("call-with-output-file : string required but got ", filepath);
    if (!SCM_FUNCP(proc) && !SCM_CLOSUREP(proc))
        SigScm_ErrorObj("call-with-output-file : proc required but got ", proc);
    
    /* open port */
    port = ScmOp_open_output_file(filepath);
    
    /* (apply proc (port)) */
    ret = ScmOp_apply(Scm_NewCons(proc,
                                  Scm_NewCons(Scm_NewCons(port, SCM_NIL),
                                              SCM_NIL)),
                      SCM_NIL);

    /* close port */
    ScmOp_close_output_port(port);

    return ret;
}

ScmObj ScmOp_input_portp(ScmObj obj)
{
    if (SCM_PORTP(obj) && SCM_PORT_PORTDIRECTION(obj) == PORT_INPUT)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_output_portp(ScmObj obj)
{
    if (SCM_PORTP(obj) && SCM_PORT_PORTDIRECTION(obj) == PORT_OUTPUT)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_current_input_port(void)
{
    return current_input_port;
}

ScmObj ScmOp_current_output_port(void)
{
    return current_output_port;
}

ScmObj ScmOp_with_input_from_file(ScmObj filepath, ScmObj thunk)
{
    ScmObj tmp_port = SCM_NIL;
    ScmObj ret      = SCM_NIL;

    if (!SCM_STRINGP(filepath))
        SigScm_ErrorObj("with-input-from-file : string required but got ", filepath);
    if (!SCM_FUNCP(thunk) && !SCM_CLOSUREP(thunk))
        SigScm_ErrorObj("with-input-from-file : proc required but got ", thunk);
    
    /* set current_input_port */
    tmp_port = current_input_port;
    current_input_port = ScmOp_open_input_file(filepath);
    
    /* (apply thunk ())*/
    ret = ScmOp_apply(Scm_NewCons(thunk,
                                  Scm_NewCons(Scm_NewCons(SCM_NIL, SCM_NIL),
                                              SCM_NIL)),
                      SCM_NIL);

    /* close port */
    ScmOp_close_input_port(current_input_port);

    /* restore current_input_port */
    current_input_port = tmp_port;

    return ret;
}

ScmObj ScmOp_with_output_to_file(ScmObj filepath, ScmObj thunk)
{
    ScmObj tmp_port = SCM_NIL;
    ScmObj ret      = SCM_NIL;

    if (!SCM_STRINGP(filepath))
        SigScm_ErrorObj("with-output-to-file : string required but got ", filepath);
    if (!SCM_FUNCP(thunk) && !SCM_CLOSUREP(thunk))
        SigScm_ErrorObj("with-output-to-file : proc required but got ", thunk);
    
    /* set current_output_port */
    tmp_port = current_output_port;
    current_output_port = ScmOp_open_output_file(filepath);
    
    /* (apply thunk ())*/
    ret = ScmOp_apply(Scm_NewCons(thunk,
                                  Scm_NewCons(Scm_NewCons(SCM_NIL, SCM_NIL),
                                              SCM_NIL)),
                      SCM_NIL);

    /* close port */
    ScmOp_close_output_port(current_output_port);

    /* restore current_output_port */
    current_output_port = tmp_port;

    return ret;
}

ScmObj ScmOp_open_input_file(ScmObj filepath)
{
    FILE *f = NULL;

    if (!SCM_STRINGP(filepath))
        SigScm_ErrorObj("open-input-file : string requred but got ", filepath);

    /* Open File */
    f = fopen(SCM_STRING_STR(filepath), "r");
    if (!f)
        SigScm_ErrorObj("open-input-file : cannot open file ", filepath);

    /* Allocate ScmPort */
    return Scm_NewFilePort(f, SCM_STRING_STR(filepath), PORT_INPUT);
}

ScmObj ScmOp_open_output_file(ScmObj filepath)
{
    FILE *f = NULL;

    if (!SCM_STRINGP(filepath))
        SigScm_ErrorObj("open-output-file : string requred but got ", filepath);

    /* Open File */
    f = fopen(SCM_STRING_STR(filepath), "w");
    if (!f)
        SigScm_ErrorObj("open-output-file : cannot open file ", filepath);

    /* Return new ScmPort */
    return Scm_NewFilePort(f, SCM_STRING_STR(filepath), PORT_OUTPUT);
}

ScmObj ScmOp_close_input_port(ScmObj port)
{
    if (!SCM_PORTP(port))
        SigScm_ErrorObj("close-input-port : port requred but got ", port);

    if (SCM_PORTINFO_FILE(port))
        fclose(SCM_PORTINFO_FILE(port));

    return SCM_UNDEF;
}

ScmObj ScmOp_close_output_port(ScmObj port)
{
    if (!SCM_PORTP(port))
        SigScm_ErrorObj("close-output-port : port requred but got ", port);
    
    if (SCM_PORTINFO_FILE(port))
        fclose(SCM_PORTINFO_FILE(port));

    return SCM_UNDEF;
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.2 Input
===========================================================================*/
ScmObj ScmOp_read(ScmObj arg, ScmObj env)
{
    ScmObj port = SCM_NIL;
    if (SCM_NULLP(arg)) {
        /* (read) */
        port = current_input_port;
    } else if (SCM_PORTP(SCM_CAR(arg))) {
        /* (read port) */
        port = SCM_CAR(arg);
    } else {
        SigScm_ErrorObj("read : invalid parameter", arg);
    }

    return SigScm_Read(port);
}

ScmObj ScmOp_read_char(ScmObj arg, ScmObj env)
{
    ScmObj port = SCM_NIL;
    char  *buf  = NULL;
    if (SCM_NULLP(arg)) {
        /* (read-char) */
        port = current_input_port;
    } else if (!SCM_NULLP(SCM_CDR(arg)) && SCM_PORTP(SCM_CAR(SCM_CDR(arg)))) {
        /* (read-char port) */
        port = SCM_CAR(SCM_CDR(arg));
    } else {
        SigScm_ErrorObj("read-char : invalid parameter", arg);
    }

    /* TODO : implement this multibyte-char awareness */
    buf = (char *)malloc(sizeof(char) * 2);
    buf[0] = getc(SCM_PORTINFO_FILE(port));
    buf[1] = '\0';
    return Scm_NewChar(buf);
}

ScmObj ScmOp_peek_char(ScmObj arg, ScmObj env)
{
    /* TODO : implement this */
}

ScmObj ScmOp_eof_objectp(ScmObj obj)
{
    if(EQ(obj, SCM_EOF))
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_readyp(ScmObj arg, ScmObj env)
{
    /* TODO : implement this */
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.3 Output
===========================================================================*/
ScmObj ScmOp_write(ScmObj arg, ScmObj env)
{
    ScmObj obj  = SCM_NIL;
    ScmObj port = SCM_NIL;

    if CHECK_1_ARG(arg)
        SigScm_Error("write : invalid parameter\n");

    /* get obj */
    obj = SCM_CAR(arg);
    arg = SCM_CDR(arg);

    /* get port */
    port = current_output_port;
    if (!SCM_NULLP(arg) && !SCM_NULLP(SCM_CAR(arg)) && SCM_PORTP(SCM_CAR(arg)))
        port = SCM_CAR(arg);

    SigScm_WriteToPort(port, obj);
    return SCM_UNDEF;
}

ScmObj ScmOp_display(ScmObj arg, ScmObj env)
{
    ScmObj obj  = SCM_NIL;
    ScmObj port = SCM_NIL;

    if CHECK_1_ARG(arg)
        SigScm_Error("display : invalid parameter\n");

    /* get obj */
    obj = SCM_CAR(arg);
    arg = SCM_CDR(arg);

    /* get port */
    port = current_output_port;
    
    /* (display obj port) */
    if (!SCM_NULLP(arg) && SCM_PORTP(SCM_CAR(arg)))
        port = SCM_CAR(arg);

    SigScm_DisplayToPort(port, obj);
    return SCM_UNDEF;
}

ScmObj ScmOp_print(ScmObj arg, ScmObj env)
{
    ScmObj obj  = SCM_NIL;
    ScmObj port = SCM_NIL;

    if CHECK_1_ARG(arg)
        SigScm_Error("print : invalid parameter\n");

    /* get obj */
    obj = SCM_CAR(arg);
    arg = SCM_CDR(arg);

    /* get port */
    port = current_output_port;
    
    /* (display obj port) */
    if (!SCM_NULLP(arg) && SCM_PORTP(SCM_CAR(arg)))
        port = SCM_CAR(arg);

    SigScm_DisplayToPort(port, obj);
    SigScm_DisplayToPort(port, Scm_NewStringCopying("\n"));
    return SCM_UNDEF;

}

ScmObj ScmOp_newline(ScmObj arg, ScmObj env)
{
    /* get port */
    ScmObj port = current_output_port;

    /* (newline port) */
    if (!SCM_NULLP(arg) && !SCM_NULLP(SCM_CAR(arg)) && SCM_PORTP(SCM_CAR(arg))) {
        port = SCM_CAR(arg);
    }

    SigScm_DisplayToPort(port, Scm_NewStringCopying("\n"));
    return SCM_UNDEF;
}

ScmObj ScmOp_write_char(ScmObj arg, ScmObj env)
{
    ScmObj obj  = SCM_NIL;
    ScmObj port = SCM_NIL;

    if CHECK_1_ARG(arg)
        SigScm_Error("write-char : invalid parameter\n");

    /* get obj */
    obj = SCM_CAR(arg);
    arg = SCM_CDR(arg);
    if (!SCM_CHARP(obj))
        SigScm_ErrorObj("write-char : char required but got ", obj);

    /* get port */
    port = current_output_port;
    
    /* (write-char obj port) */
    if (!SCM_NULLP(arg) && SCM_PORTP(SCM_CAR(arg)))
        port = SCM_CAR(arg);

    SigScm_DisplayToPort(port, obj);
    return SCM_UNDEF;
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.4 System Interface
===========================================================================*/
ScmObj SigScm_load(const char *filename)
{
    ScmObj stack_start;
    ScmObj port         = SCM_NIL;
    ScmObj s_expression = SCM_NIL;
    char  *c_filename = strdup(filename);
    char  *filepath = NULL;

    /* start protecting stack */
    SigScm_gc_protect_stack(&stack_start);

    /* construct filepath */
    if (lib_path) {
        filepath = alloca(strlen(lib_path) + strlen(c_filename) + 2);
        strcpy(filepath, lib_path);
        strcat(filepath, "/");
        strcat(filepath, c_filename);
    } else {
        filepath = alloca(strlen(c_filename) + 1);
        strcpy(filepath, c_filename);
    }

    /* open port */
    port = ScmOp_open_input_file(Scm_NewStringCopying(filepath));
    s_expression = SCM_NIL;
    
    /* read & eval cycle */
    for (s_expression = SigScm_Read(port);
         !EQ(s_expression, SCM_EOF);
         s_expression = SigScm_Read(port))
    {
        ScmOp_eval(s_expression, SCM_NIL);
    }

    /* close port */
    ScmOp_close_input_port(port);

    /* now no need to protect stack */
    SigScm_gc_unprotect_stack(&stack_start);

    /* free str */
    free(c_filename);

    return SCM_UNSPECIFIED;
}

ScmObj ScmOp_load(ScmObj filename)
{
    char *c_filename = SCM_STRING_STR(filename);
    SigScm_load(c_filename);

    /* TODO : investigate */
    return SCM_TRUE;
}

ScmObj ScmOp_require(ScmObj filename)
{
    ScmObj stack_start;
    ScmObj loaded_str = SCM_NIL;

    if (!SCM_STRINGP(filename))
        SigScm_ErrorObj("require : string required but got ", filename);

    /* start protecting stack */
    SigScm_gc_protect_stack(&stack_start);

    /* construct loaded_str */
    loaded_str = create_loaded_str(filename);

    if (EQ(ScmOp_member(loaded_str, SCM_SYMBOL_VCELL(SigScm_features)), SCM_FALSE)) {
        /* not provided, so load it! */
        ScmOp_load(filename);

        /* record to SigScm_features */
        SCM_SYMBOL_VCELL(SigScm_features) = Scm_NewCons(loaded_str, SCM_SYMBOL_VCELL(SigScm_features));
    }

    /* now no need to protect stack */
    SigScm_gc_unprotect_stack(&stack_start);

    return SCM_TRUE;
}

static ScmObj create_loaded_str(ScmObj filename)
{
    char  *loaded_str = NULL;
    int    size = 0;

    /* generate loaded_str, contents is filename-loaded* */
    size = (strlen(SCM_STRING_STR(filename)) + strlen("-loaded*") + 1);
    loaded_str = (char*)malloc(sizeof(char) * size);
    snprintf(loaded_str, size, "%s-loaded*", SCM_STRING_STR(filename));
    
    return Scm_NewString(loaded_str);
}

ScmObj ScmOp_provide(ScmObj feature)
{
    if (!SCM_STRINGP(feature))
        SigScm_ErrorObj("provide : string required but got ", feature);

    /* record to SigScm_features */
    SCM_SYMBOL_VCELL(SigScm_features) = Scm_NewCons(feature, SCM_SYMBOL_VCELL(SigScm_features));

    return SCM_TRUE;
}

ScmObj ScmOp_providedp(ScmObj feature)
{
    if (!SCM_STRINGP(feature))
        SigScm_ErrorObj("provide : string required but got ", feature);

    if (EQ(ScmOp_member(feature, SigScm_features), SCM_TRUE))
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_file_existsp(ScmObj filepath)
{
    FILE *f = NULL;

    if (!SCM_STRINGP(filepath))
        SigScm_ErrorObj("file-exists? : string requred but got ", filepath);

    f = fopen(SCM_STRING_STR(filepath), "r");
    if (!f)
        return SCM_FALSE;

    fclose(f);

    return SCM_TRUE;
}

ScmObj ScmOp_delete_file(ScmObj filepath)
{
    if (!SCM_STRINGP(filepath))
        SigScm_ErrorObj("delete-file : string requred but got ", filepath);

    if (remove(SCM_STRING_STR(filepath)) == -1)
        SigScm_ErrorObj("delete-file : delete failed. file = ", filepath);
    
    return SCM_TRUE;
}
