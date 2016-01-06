/*
  Copyright (c) 2008-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. Neither the name of authors nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <expat.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-helper.h"
#include "uim-notify.h"
#include "dynlib.h"

typedef struct {
  uim_lisp start_;
  uim_lisp end_;
  uim_lisp characterdata_;
} uim_xml_userdata;

typedef struct {
  XML_Parser parser;
  uim_xml_userdata *data;
} uim_xml_ctx;

static uim_lisp
xml_start_element_handler_internal(const XML_Char *atts[])
{
  uim_lisp atts_ = uim_scm_null();

  while (*atts != '\0') {
    atts_ = CONS(CONS(MAKE_STR(*atts), MAKE_STR(*(atts + 1))), atts_);
    atts += 2;
  }
  return atts_;
}

static void
xml_start_element_handler(void *userData, const XML_Char *name, const XML_Char *atts[])
{
  uim_xml_userdata *data = (uim_xml_userdata *)userData;

  if (data && data->start_) {
    uim_lisp atts_;

    atts_ = (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)xml_start_element_handler_internal,
						       (void *)atts);

    atts_ = uim_scm_callf("reverse", "o", atts_);

    uim_scm_call(data->start_, LIST2(MAKE_STR(name), atts_));
  }
}

static void
xml_end_element_handler(void *userData, const XML_Char *name)
{
  uim_xml_userdata *data = (uim_xml_userdata *)userData;

  if (data && data->end_) {
    uim_scm_call(data->end_, LIST1(MAKE_STR(name)));
  }
}

static void
xml_characterdata_handler(void *userData, const XML_Char *s, int len)
{
  uim_xml_userdata *data = (uim_xml_userdata *)userData;

  char *str = uim_malloc(len + 1);

  memcpy(str, s, len);
  str[len] = '\0';

  if (data && data->characterdata_) {
    uim_scm_call(data->characterdata_, LIST1(MAKE_STR(str)));
  }

  free(str);
}

static uim_lisp
uim_xml_parser_create(uim_lisp encoding_)
{
  uim_xml_ctx *ctx;
  const XML_Char *encoding = REFER_C_STR(encoding_);
  XML_Parser parser;

  parser = XML_ParserCreate(encoding);

  if (parser) {
    XML_SetElementHandler(parser, xml_start_element_handler, xml_end_element_handler);
    XML_SetCharacterDataHandler(parser, xml_characterdata_handler);
  }

  ctx = uim_calloc(1, sizeof(uim_xml_ctx *));
  ctx->parser = parser;

  ctx->data = uim_malloc(sizeof(uim_xml_userdata *));
  ctx->data->start_ = NULL;
  ctx->data->end_ = NULL;
  ctx->data->characterdata_ = NULL;

  return MAKE_PTR(ctx);
}

static uim_lisp
uim_xml_parser_free(uim_lisp ctx_)
{
  uim_xml_ctx *ctx = C_PTR(ctx_);

  free(ctx->data);
  XML_ParserFree(ctx->parser);
  free(ctx);

  return uim_scm_t();
}

static uim_lisp
uim_xml_set_element_handler(uim_lisp ctx_, uim_lisp element_start_, uim_lisp element_end_)
{
  uim_xml_ctx *ctx = C_PTR(ctx_);

  ctx->data->start_ = element_start_;
  ctx->data->end_   = element_end_;

  return uim_scm_t();
}

static uim_lisp
uim_xml_set_characterdata_handler(uim_lisp ctx_, uim_lisp element_characterdata_)
{
  uim_xml_ctx *ctx = C_PTR(ctx_);

  ctx->data->characterdata_ = element_characterdata_;

  return uim_scm_t();
}

struct uim_xml_parse_args {
  uim_lisp ctx_;
  uim_lisp s_;
  uim_lisp isFinal_;
};

static void *uim_xml_parse_internal(struct uim_xml_parse_args *);

static uim_lisp
uim_xml_parse(uim_lisp ctx_, uim_lisp s_, uim_lisp isFinal_)
{
  struct uim_xml_parse_args args;

  args.ctx_ = ctx_;
  args.s_ = s_;
  args.isFinal_ = isFinal_;

  return uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_xml_parse_internal, (void *)&args);
}

static void *
uim_xml_parse_internal(struct uim_xml_parse_args *args)
{
  uim_xml_ctx *ctx = C_PTR(args->ctx_);
  const XML_Char *s = REFER_C_STR(args->s_);
  int isFinal = C_INT(args->isFinal_);

  XML_SetUserData(ctx->parser, ctx->data);
  return MAKE_INT(XML_Parse(ctx->parser, s, strlen(s), isFinal));
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc1("xml-parser-create", uim_xml_parser_create);
  uim_scm_init_proc1("xml-parser-free", uim_xml_parser_free);
  uim_scm_init_proc3("xml-element-handler-set!", uim_xml_set_element_handler);
  uim_scm_init_proc2("xml-characterdata-handler-set!", uim_xml_set_characterdata_handler);
  uim_scm_init_proc3("xml-parse", uim_xml_parse);
}

void
uim_plugin_instance_quit(void)
{
}
