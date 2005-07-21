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

/*=======================================
  File Local Function Declarations
=======================================*/
ScmObj current_input_port  = NULL;
ScmObj current_output_port = NULL;

/*=======================================
  Function Implementations
=======================================*/
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
    
    /* (eval '(proc port) '())*/
    ret = ScmOp_eval(Scm_NewCons(proc, Scm_NewCons(port, SCM_NIL)), SCM_NIL);

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
    
    /* (eval '(proc port) '())*/
    ret = ScmOp_eval(Scm_NewCons(proc, Scm_NewCons(port, SCM_NIL)), SCM_NIL);

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
    
    /* (eval '(thunk) '())*/
    ret = ScmOp_eval(Scm_NewCons(thunk, SCM_NIL), SCM_NIL);

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
    
    /* (eval '(thunk) '())*/
    ret = ScmOp_eval(Scm_NewCons(thunk, SCM_NIL), SCM_NIL);

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
    return Scm_NewPort(f, PORT_INPUT, PORT_FILE);
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
    return Scm_NewPort(f, PORT_OUTPUT, PORT_FILE);
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
    } else if (!SCM_NULLP(SCM_CDR(arg)) && SCM_PORTP(SCM_CAR(SCM_CDR(arg)))) {
	/* (read port) */
	port = SCM_CAR(SCM_CDR(arg));
    } else {
	SigScm_ErrorObj("read : invalid paramter", arg);
    }

    return SigScm_Read(port);
}

ScmObj ScmOp_read_char(ScmObj arg, ScmObj env)
{
    ScmObj port = SCM_NIL;
    if (SCM_NULLP(arg)) {
	/* (read-char) */
	port = current_input_port;
    } else if (!SCM_NULLP(SCM_CDR(arg)) && SCM_PORTP(SCM_CAR(SCM_CDR(arg)))) {
	/* (read-char port) */
	port = SCM_CAR(SCM_CDR(arg));
    } else {
	SigScm_ErrorObj("read-char : invalid paramter", arg);
    }

    return SigScm_Read_Char(port);
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

/*
 * TODO : implement this properly!!!
 */
ScmObj ScmOp_write(ScmObj arg, ScmObj env)
{
    ScmObj obj  = SCM_NIL;
    ScmObj port = SCM_NIL;

    if CHECK_1_ARG(arg)
	SigScm_Error("write : invalid paramter\n");

    /* get obj */
    obj = SCM_CAR(arg);
    arg = SCM_CDR(arg);

    /* get port */
    port = SCM_NIL;
    if (SCM_NULLP(arg)) {
	/* (write obj) */
	port = current_input_port;
    } else if (!SCM_NULLP(SCM_CDR(arg)) && SCM_PORTP(SCM_CAR(SCM_CDR(arg)))) {
	/* (write obj port) */
	port = SCM_CAR(SCM_CDR(arg));
    } else {
	SigScm_ErrorObj("write : invalid paramter ", arg);
    }

    SigScm_DisplayToPort(port, obj);
    return SCM_UNDEF;
}

/*
 * TODO : implement this properly!!!
 */
ScmObj ScmOp_display(ScmObj arg, ScmObj env)
{
    ScmObj obj  = SCM_NIL;
    ScmObj port = SCM_NIL;

    if CHECK_1_ARG(arg)
	SigScm_Error("display : invalid paramter\n");

    /* get obj */
    obj = SCM_CAR(arg);
    arg = SCM_CDR(arg);

    /* get port */
    port = SCM_NIL;
    if (SCM_NULLP(arg)) {
	/* (write obj) */
	port = current_output_port;
    } else if (!SCM_NULLP(SCM_CDR(arg)) && SCM_PORTP(SCM_CAR(SCM_CDR(arg)))) {
	/* (write obj port) */
	port = SCM_CAR(SCM_CDR(arg));
    } else {
	SigScm_ErrorObj("display : invalid paramter ", arg);
    }

    SigScm_DisplayToPort(port, obj);
    return SCM_UNDEF;
}

ScmObj ScmOp_newline(ScmObj arg, ScmObj env)
{
    /* get port */
    ScmObj port = SCM_NIL;
    if (SCM_NULLP(arg)) {
	/* (write obj) */
	port = current_output_port;
    } else if (!SCM_NULLP(SCM_CDR(arg)) && SCM_PORTP(SCM_CAR(SCM_CDR(arg)))) {
	/* (write obj port) */
	port = SCM_CAR(SCM_CDR(arg));
    } else {
	SigScm_ErrorObj("newline : invalid paramter ", arg);
    }

    fprintf(SCM_PORTINFO_FILE(port), "\n");
    return SCM_UNDEF;
}

ScmObj ScmOp_write_char(ScmObj arg, ScmObj env)
{
    ScmObj obj  = SCM_NIL;
    ScmObj port = SCM_NIL;

    if CHECK_1_ARG(arg)
	SigScm_Error("write-char : invalid paramter\n");

    /* get obj */
    obj = SCM_CAR(arg);
    arg = SCM_CDR(arg);
    if (!SCM_CHARP(obj))
	SigScm_ErrorObj("write-char : char required but got ", obj);

    /* get port */
    port = SCM_NIL;
    if (SCM_NULLP(arg)) {
	/* (write obj) */
	port = current_input_port;
    } else if (!SCM_NULLP(SCM_CDR(arg)) && SCM_PORTP(SCM_CAR(SCM_CDR(arg)))) {
	/* (write obj port) */
	port = SCM_CAR(SCM_CDR(arg));
    } else {
	SigScm_ErrorObj("write : invalid paramter ", arg);
    }

    SigScm_DisplayToPort(port, obj);
    return SCM_UNDEF;
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.4 System Interface
===========================================================================*/
ScmObj SigScm_load(char *c_filename)
{
    ScmObj stack_start;
    ScmObj port         = SCM_NIL;
    ScmObj s_expression = SCM_NIL;

    /* set stack start */
    stack_start_pointer = &stack_start;

    /* open port */
    port = ScmOp_open_input_file(Scm_NewString(c_filename));
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

    stack_start_pointer = NULL;

    return SCM_UNSPECIFIED;
}

ScmObj ScmOp_load(ScmObj filename)
{
    char *c_filename = SCM_STRING_STR(filename);
    SigScm_load(c_filename);

    /* TODO : investigate */
    return SCM_NIL;
}

