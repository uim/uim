/*
 *  $Id:$
 *  Copyright (c) 2003,2004 Masahito Omote <omote@utyuuzin.net>
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
 *  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#ifdef USE_DLOPEN
#include <dlfcn.h>
#endif

#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "uim/uim.h"
#include "uim/uim-compat-scm.h"
#include "uim/uim-compat-custom.h"
#include "uim/uim-helper.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "uimconfig.h"

static FILE *siod_output = NULL;
static const char custom_msg_tmpl[] = "prop_update_custom\n%s\n%s\n";
static int helper_fd = -1;

/* gc_protect'ed variables */
static uim_lisp val_f;
static uim_lisp groups, group_sym, customs, custom_sym;

#ifdef USE_DLOPEN
#ifndef _uim_scm_h_included_
typedef void *uim_lisp; /* from uim-scm.h by uim project */
#endif

#define SYMBOL_EQ(custom_sym, str) \
          uim_api.uim_scm_eq(custom_sym, \
			     uim_api.uim_scm_intern_c_str(str))
#else
#define SYMBOL_EQ(custom_sym, str) \
          uim_scm_eq(custom_sym, uim_scm_intern_c_str(str))
#endif



int    uimapi_init     (void);
int    uimapi_close    (void);
int    init_uim_config (struct _uim_config *config);
void   free_uim_config (struct _uim_config *config);
void   uimapi_gc_protect(uim_lisp *location);

static void read_uim_config_global (struct _uim_config_global *config);
static void read_uim_config_canna  (struct _uim_config_canna  *config);
static void read_uim_config_skk	   (struct _uim_config_skk    *config);
static void free_uim_config_global (struct _uim_config_global *config);
static void free_uim_config_canna  (struct _uim_config_canna  *config);
static void free_uim_config_skk	   (struct _uim_config_skk    *config);

static void helper_disconnect_cb(void);

#ifdef USE_DLOPEN
#ifndef _uim_scm_h_included_
static struct _uim_api uim_api;
#else
struct uim_api_tbl uim_api;
#endif
static void   *uim_lib;
#endif

int uimapi_init (void) {
#ifdef USE_DLOPEN
    uim_lib = dlopen("libuim.so.0", RTLD_GLOBAL | RTLD_NOW);
    if(uim_lib == NULL)
	return -1;

    memset(&uim_api, 0, sizeof(struct uim_api_tbl));

    uim_api.uim_init = dlsym(uim_lib, "uim_init");
    uim_api.uim_quit = dlsym(uim_lib, "uim_quit");

    uim_api.uim_scm_get_output = dlsym(uim_lib, "uim_scm_get_output");
    uim_api.uim_scm_set_output = dlsym(uim_lib, "uim_scm_set_output");
    uim_api.uim_scm_get_verbose_level = dlsym(uim_lib, "uim_scm_get_verbose_level");
    uim_api.uim_scm_set_verbose_level = dlsym(uim_lib, "uim_scm_set_verbose_level");
    uim_api.uim_scm_load_file = dlsym(uim_lib, "uim_scm_load_file");

    uim_api.uim_scm_c_int = dlsym(uim_lib, "uim_scm_c_int");
    uim_api.uim_scm_symbol_value_int = dlsym(uim_lib, "uim_scm_symbol_value_int");
    uim_api.uim_scm_int_from_c_int = dlsym(uim_lib, "uim_scm_int_from_c_int");
    uim_api.uim_scm_c_str = dlsym(uim_lib, "uim_scm_c_str");
    uim_api.uim_scm_symbol_value_str = dlsym(uim_lib, "uim_scm_symbol_value_str");
    uim_api.uim_scm_str_from_c_str = dlsym(uim_lib, "uim_scm_str_from_c_str");
    uim_api.uim_scm_c_strs_into_list = dlsym(uim_lib, "uim_scm_c_strs_into_list");
    uim_api.uim_scm_symbol_value = dlsym(uim_lib, "uim_scm_symbol_value");
    uim_api.uim_scm_intern_c_str = dlsym(uim_lib, "uim_scm_intern_c_str");
    uim_api.uim_scm_qintern_c_str = dlsym(uim_lib, "uim_scm_qintern_c_str");
    uim_api.uim_scm_gc_protect = dlsym(uim_lib, "uim_scm_gc_protect");
    uim_api.uim_scm_repl_c_string = dlsym(uim_lib, "uim_scm_repl_c_string");

    uim_api.uim_scm_t = dlsym(uim_lib, "uim_scm_t");
    uim_api.uim_scm_f = dlsym(uim_lib, "uim_scm_f");
    uim_api.uim_scm_null_list = dlsym(uim_lib, "uim_scm_null_list");
    uim_api.uim_scm_nullp = dlsym(uim_lib, "uim_scm_nullp");
    uim_api.uim_scm_eq = dlsym(uim_lib, "uim_scm_eq");
    uim_api.uim_scm_string_equal = dlsym(uim_lib, "uim_scm_string_equal");
    uim_api.uim_scm_eval = dlsym(uim_lib, "uim_scm_eval");
    uim_api.uim_scm_quote = dlsym(uim_lib, "uim_scm_quote");
    uim_api.uim_scm_car = dlsym(uim_lib, "uim_scm_car");
    uim_api.uim_scm_cdr = dlsym(uim_lib, "uim_scm_cdr");
    uim_api.uim_scm_cadr = dlsym(uim_lib, "uim_scm_cadr");
    uim_api.uim_scm_caar = dlsym(uim_lib, "uim_scm_caar");
    uim_api.uim_scm_cdar = dlsym(uim_lib, "uim_scm_cdar");
    uim_api.uim_scm_cddr = dlsym(uim_lib, "uim_scm_cddr");
    uim_api.uim_scm_cons = dlsym(uim_lib, "uim_scm_cons");
    uim_api.uim_scm_nth = dlsym(uim_lib, "uim_scm_nth");
    uim_api.uim_scm_list1 = dlsym(uim_lib, "uim_scm_list1");
    uim_api.uim_scm_list2 = dlsym(uim_lib, "uim_scm_list2");
    uim_api.uim_scm_list3 = dlsym(uim_lib, "uim_scm_list3");
    uim_api.uim_scm_list4 = dlsym(uim_lib, "uim_scm_list4");
    uim_api.uim_scm_list5 = dlsym(uim_lib, "uim_scm_list5");

    uim_api.uim_custom_value = dlsym(uim_lib, "uim_custom_value");
    uim_api.uim_custom_value_as_bool = dlsym(uim_lib, "uim_custom_value_as_bool");
    uim_api.uim_custom_value_as_int = dlsym(uim_lib, "uim_custom_value_as_int");
    uim_api.uim_custom_value_as_str = dlsym(uim_lib, "uim_custom_value_as_str");
    uim_api.uim_custom_value_as_path = dlsym(uim_lib, "uim_custom_value_as_path");
    uim_api.uim_custom_value_as_symbol = dlsym(uim_lib, "uim_custom_value_as_symbol");
    uim_api.uim_custom_set = dlsym(uim_lib, "uim_custom_set");
    uim_api.uim_custom_symbol_label = dlsym(uim_lib, "uim_custom_symbol_label");
    uim_api.uim_custom_symbol_desc = dlsym(uim_lib, "uim_custom_symbol_desc");
    uim_api.uim_custom_label = dlsym(uim_lib, "uim_custom_label");
    uim_api.uim_custom_desc = dlsym(uim_lib, "uim_custom_desc");
    uim_api.uim_custom_default_value = dlsym(uim_lib, "uim_custom_default_value");
    uim_api.uim_custom_type = dlsym(uim_lib, "uim_custom_type");
    uim_api.uim_custom_ctype = dlsym(uim_lib, "uim_custom_ctype");
    uim_api.uim_custom_range = dlsym(uim_lib, "uim_custom_range");
    uim_api.uim_custom_group_label = dlsym(uim_lib, "uim_custom_group_label");
    uim_api.uim_custom_group_desc = dlsym(uim_lib, "uim_custom_group_desc");
    uim_api.uim_custom_group_subgroups = dlsym(uim_lib, "uim_custom_group_subgroups");
    uim_api.uim_custom_list_groups = dlsym(uim_lib, "uim_custom_list_groups");
    uim_api.uim_custom_list_primary_groups = dlsym(uim_lib, "uim_custom_list_primary_groups");
    uim_api.uim_custom_collect_by_group = dlsym(uim_lib, "uim_custom_collect_by_group");
    uim_api.uim_custom_definition_as_string = dlsym(uim_lib, "uim_custom_definition_as_string");
    uim_api.uim_scm_reverse = dlsym(uim_lib, "uim_scm_reverse");
    uim_api.uim_scm_nreverse = dlsym(uim_lib,"uim_scm_nreverse");

    if (!uim_api.uim_init ||
	!uim_api.uim_quit ||
	!uim_api.uim_scm_get_output ||
	!uim_api.uim_scm_set_output ||
	!uim_api.uim_scm_get_verbose_level ||
	!uim_api.uim_scm_set_verbose_level ||
	!uim_api.uim_scm_load_file ||
	!uim_api.uim_scm_c_int ||
	!uim_api.uim_scm_symbol_value_int ||
	!uim_api.uim_scm_int_from_c_int ||
	!uim_api.uim_scm_c_str ||
	!uim_api.uim_scm_symbol_value_str ||
	!uim_api.uim_scm_str_from_c_str ||
	!uim_api.uim_scm_c_strs_into_list ||
	!uim_api.uim_scm_symbol_value ||
	!uim_api.uim_scm_intern_c_str ||
	!uim_api.uim_scm_qintern_c_str ||
	!uim_api.uim_scm_gc_protect ||
	!uim_api.uim_scm_repl_c_string ||
	!uim_api.uim_scm_t ||
	!uim_api.uim_scm_f ||
	!uim_api.uim_scm_null_list ||
	!uim_api.uim_scm_nullp ||
	!uim_api.uim_scm_eq ||
	!uim_api.uim_scm_string_equal ||
	!uim_api.uim_scm_eval ||
	!uim_api.uim_scm_quote ||
	!uim_api.uim_scm_car ||
	!uim_api.uim_scm_cadr ||
	!uim_api.uim_scm_caar ||
	!uim_api.uim_scm_cdar ||
	!uim_api.uim_scm_cddr ||
	!uim_api.uim_scm_cdr ||
	!uim_api.uim_scm_cons ||
	!uim_api.uim_scm_nth ||
	!uim_api.uim_scm_list1 ||
	!uim_api.uim_scm_list2 ||
	!uim_api.uim_scm_list3 ||
	!uim_api.uim_scm_list4 ||
	!uim_api.uim_scm_list5 ||
	!uim_api.uim_custom_value ||
	!uim_api.uim_custom_value_as_bool ||
	!uim_api.uim_custom_value_as_int ||
	!uim_api.uim_custom_value_as_str ||
	!uim_api.uim_custom_value_as_path ||
	!uim_api.uim_custom_value_as_symbol ||
	!uim_api.uim_custom_set ||
	!uim_api.uim_custom_symbol_label ||
	!uim_api.uim_custom_symbol_desc ||
	!uim_api.uim_custom_label ||
	!uim_api.uim_custom_desc ||
	!uim_api.uim_custom_default_value ||
	!uim_api.uim_custom_type ||
	!uim_api.uim_custom_ctype ||
	!uim_api.uim_custom_range ||
	!uim_api.uim_custom_group_label ||
	!uim_api.uim_custom_group_desc ||
	!uim_api.uim_custom_group_subgroups ||
	!uim_api.uim_custom_list_groups ||
	!uim_api.uim_custom_list_primary_groups ||
	!uim_api.uim_custom_collect_by_group ||
	!uim_api.uim_custom_definition_as_string ||
	!uim_api.uim_scm_reverse ||
	!uim_api.uim_scm_nreverse
	) {
	dlclose(uim_lib);
	return -1;
    }
    return 0;
#else
    return 0;
#endif
}

int uimapi_close(void) {
#ifdef USE_DLOPEN
    if (uim_lib)
	return dlclose(uim_lib);
    else
	return -1;
#endif
    return 0;
}

int init_uim_config(struct _uim_config *config)
{
    struct _uim_config_global *conf_global;
    struct _uim_config_skk    *conf_skk;
    struct _uim_config_canna  *conf_canna;

    conf_global = &(config->global);
    conf_skk	= &(config->skk);
    conf_canna	= &(config->canna);

#if 0
    siod_output = fopen("/tmp/siod.out", "w");
    uim_api.uim_scm_set_output(siod_output);
#endif

#ifdef USE_DLOPEN
    uim_api.uim_init();
    val_f = uim_api.uim_scm_f();
    uim_api.uim_scm_load_file("custom.scm");
#else
    uim_init();
    val_f = uim_scm_f();
    uim_scm_load_file("custom.scm");
#endif

/*    read_uim_config_global(conf_global);
    read_uim_config_anthy();
    read_uim_config_prime();
    read_uim_config_skk(conf_skk);
    read_uim_config_canna(conf_canna); */

    uimapi_gc_protect(&val_f);
    uimapi_gc_protect(&groups);
    uimapi_gc_protect(&group_sym);
    uimapi_gc_protect(&customs);
    uimapi_gc_protect(&custom_sym);

    return 0;
}

void free_uim_config(struct _uim_config *config)
{
    free_uim_config_global(&(config->global));
/*    free_uim_config_anthy(&(config->anthy)); */
/*    free_uim_config_prime(&(config->prime)); */
    free_uim_config_canna(&(config->canna));
    free_uim_config_skk(&(config->skk));

#if 0
    fclose(siod_output);
#endif
#ifdef USE_DLOPEN
    uim_api.uim_quit();
#else
    uim_quit();
#endif
}

void uimapi_gc_protect(uim_lisp *location)
{
#ifdef USE_DLOPEN
    uim_api.uim_scm_gc_protect(location);
#else
    uim_scm_gc_protect(location);
#endif
}

static void read_uim_config_global(struct _uim_config_global *config)
{
/* XXX */
#ifdef USE_DLOPEN
    uim_lisp group, customs, custom_sym;

    group = uim_api.uim_scm_intern_c_str("global");
    for(customs = uim_api.uim_custom_collect_by_group(group);
	!uim_api.uim_scm_nullp(customs);
	customs = uim_api.uim_scm_cdr(customs))
    {
	custom_sym = uim_api.uim_scm_car(customs);
	if(SYMBOL_EQ(custom_sym, "custom-preserved-default-im-name")) {
	    uim_lisp current_im_sym;

	    current_im_sym = uim_api.uim_custom_value_as_symbol(custom_sym);
#if 1
	    config->default_im = uim_api.uim_custom_symbol_label(custom_sym, current_im_sym);
#else
	    config->default_im = uim_api.uim_scm_c_str(current_im_sym);
#endif
	} else if(SYMBOL_EQ(custom_sym, "candidate-window-position")) {
	    config->cand_win_pos = uim_api.uim_custom_value_as_str(custom_sym);
	} else if(SYMBOL_EQ(custom_sym, "enable-im-switch")) {
	    config->enable_im_switch = uim_api.uim_custom_value_as_bool(custom_sym);
	}
    }
#endif
}

static void free_uim_config_global(struct _uim_config_global *config)
{
    if(config->default_im != NULL)
	free(config->default_im);
    if(config->cand_win_pos != NULL)
	free(config->cand_win_pos);
}

static void read_uim_config_skk(struct _uim_config_skk *config)
{
/* XXX */
#ifdef USE_DLOPEN
    uim_lisp group, customs, custom_sym;

    group = uim_api.uim_scm_intern_c_str("skk");
    for(customs = uim_api.uim_custom_collect_by_group(group);
	!uim_api.uim_scm_nullp(customs);
	customs = uim_api.uim_scm_cdr(customs))
    {
	custom_sym = uim_api.uim_scm_car(customs);
	if(SYMBOL_EQ(custom_sym, "skk-style")) {
	    uim_lisp current_style;

	    current_style = uim_api.uim_custom_value_as_symbol(custom_sym);
#if 1
	    config->skk_style = uim_api.uim_custom_symbol_label(custom_sym, current_style);
#else
	    config->skk_style = uim_api.uim_scm_c_str(current_style);
#endif
	} else if(SYMBOL_EQ(custom_sym, "skk-use-recursive-learning?")) {
	    config->recursive_learning = uim_api.uim_custom_value_as_bool(custom_sym);
	} else if(SYMBOL_EQ(custom_sym, "skk-dic-file-name")) {
	    config->skk_dic_filename = uim_api.uim_custom_value_as_path(custom_sym);
	} else if(SYMBOL_EQ(custom_sym, "skk-personal-dic-filename")) {
	    config->skk_userdic_filename = uim_api.uim_custom_value_as_path(custom_sym);
	} else if(SYMBOL_EQ(custom_sym, "skk-uim-personal-dic-filename")) {
	    config->skk_uim_userdic_filename = uim_api.uim_custom_value_as_path(custom_sym);
	}
    }
#endif
}

static void free_uim_config_skk(struct _uim_config_skk *config)
{
    if(config->skk_dic_filename != NULL)
	free(config->skk_dic_filename);
    if(config->skk_userdic_filename != NULL)
	free(config->skk_userdic_filename);
    if(config->skk_uim_userdic_filename != NULL)
	free(config->skk_uim_userdic_filename);
    if(config->skk_style != NULL)
	free(config->skk_style);
}

static void read_uim_config_canna(struct _uim_config_canna *config)
{
/* XXX */
#ifdef USE_DLOPEN
    uim_lisp group, customs, custom_sym;
    

    group = uim_api.uim_scm_intern_c_str("canna");
    for(customs = uim_api.uim_custom_collect_by_group(group);
	!uim_api.uim_scm_nullp(customs);
	customs = uim_api.uim_scm_cdr(customs))
    {
	custom_sym = uim_api.uim_scm_car(customs);
	if(SYMBOL_EQ(custom_sym, "canna-server-name")) {
	    if(!uim_api.uim_scm_eq(uim_api.uim_custom_value(custom_sym),
			   val_f))
	    {
		config->cannaserver = uim_api.uim_custom_value_as_str(custom_sym);
	    }
	}
    }
#endif
}

static void free_uim_config_canna(struct _uim_config_canna *config)
{
    if(config->cannaserver != NULL)
	free(config->cannaserver);
}

static void helper_disconnect_cb(void)
{
    helper_fd = -1;
}

int write_uim_config(struct _uim_config *config)
{
    FILE *fp;
    char fn[1024];
    struct stat st;
    struct passwd *pw;
    /* uim_lisp groups, group_sym, customs, custom_sym; */
    char *group_str, *custom_val, *custom_def, *msg;

    pw = getpwuid(getuid());
    snprintf(fn, sizeof(fn), "%s/.uim", pw->pw_dir);

    if(helper_fd < 0) {
	helper_fd = uim_helper_init_client_fd(helper_disconnect_cb);
    }

    fp = fopen(fn, "w");

    if(fp == NULL)
	return -1;
#ifdef USE_DLOPEN
    for(groups = uim_api.uim_custom_list_primary_groups();
	!uim_api.uim_scm_nullp(groups);
	groups = uim_api.uim_scm_cdr(groups))
    {
	group_sym = uim_api.uim_scm_car(groups);
	group_str = uim_api.uim_scm_c_str(group_sym);
	fprintf(fp, ";;;\n;;; %s\n;;;\n", group_str);
	free(group_str);

	for(customs = uim_api.uim_custom_collect_by_group(group_sym);
	    !uim_api.uim_scm_nullp(customs);
	    customs = uim_api.uim_scm_cdr(customs))
	{
	    char *custom_sym_str;

	    custom_sym = uim_api.uim_scm_car(customs);
	    custom_sym_str = uim_api.uim_scm_c_str(custom_sym);

	    msg = malloc(sizeof(custom_msg_tmpl) + strlen(custom_sym_str));
	    custom_val = uim_api.uim_custom_value_as_string(custom_sym);
	    sprintf(msg, custom_msg_tmpl, custom_sym_str, custom_val);
	    uim_helper_send_message(helper_fd, msg);
	    free(msg);
	    free(custom_val);
	    free(custom_sym_str);

	    custom_def = uim_api.uim_custom_definition_as_string(custom_sym);
	    fprintf(fp, "%s\n", custom_def);
	    printf("custom_def: %s\n", custom_def);
	    free(custom_def);
	}

	fprintf(fp, "\n");
    }
#else
    for(groups = uim_custom_list_primary_groups();
	!uim_scm_nullp(groups);
	groups = uim_scm_cdr(groups))
    {
	group_sym = uim_scm_car(groups);
	group_str = uim_scm_c_str(group_sym);
	fprintf(fp, ";;;\n;;; %s\n;;;\n", group_str);
	free(group_str);

	for(customs = uim_custom_collect_by_group(group_sym);
	    !uim_scm_nullp(customs);
	    customs = uim_scm_cdr(customs))
	{
	    char *custom_sym_str;

	    custom_sym = uim_scm_car(customs);
	    custom_sym_str = uim_scm_c_str(custom_sym);

#if 0
	    /* Exist for debugging. This code fragment should be
	       removed after the SEGV problem has been fixed.
	    */
	    if(SYMBOL_EQ(custom_sym, "custom-preserved-default-im-name"))
		break;
#endif

	    msg = malloc(sizeof(custom_msg_tmpl) + strlen(custom_sym_str));
	    custom_val = uim_custom_value_as_string(custom_sym);
	    sprintf(msg, custom_msg_tmpl, custom_sym_str, custom_val);
	    uim_helper_send_message(helper_fd, msg);
	    free(msg);
	    free(custom_val);
	    free(custom_sym_str);

	    custom_def = uim_custom_definition_as_string(custom_sym);
	    printf("custom_def: %s\n", custom_def);
	    fprintf(fp, "%s\n", custom_def);

	    free(custom_def);
	}

	fprintf(fp, "\n");
    }
#endif

    fclose(fp);
    if (helper_fd != -1) {
	uim_helper_close_client_fd(helper_fd);
    }
    return 0;
}
