/*

  Copyright (c) 2004-2012 uim Project http://code.google.com/p/uim/

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

#define Uses_SCIM_CONFIG_PATH
#define Uses_SCIM_IMENGINE_MODULE
#define Uses_SCIM_BACKEND
#define Uses_SCIM_PANEL
#define Uses_SCIM_SOCKET
#define Uses_SCIM_SOCKET_TRANSACTION
#define Uses_C_STRING

#include <scim.h>
#include "uim.h"
#include "uim-scm.h"
#include "uim-internal.h"
#include "uim-notify.h"

// FIXME: unconditional fprintf()s

using namespace scim;

typedef struct SCIMInputMethod SCIMInputMethod;
struct SCIMInputMethod {
    WideString imname;
    String lang;
    String uuid;
};

typedef struct SCIMContext SCIMContext;
struct SCIMContext {
    IMEngineFactoryPointer factory;
    IMEngineInstancePointer instance;
    int id;

    bool is_on;

    WideString preedit_str;
    AttributeList preedit_attr;
    int preedit_caret;

    int candidate_num;
    char **candidates;
};

static int initialized = 0;

static std::vector<String> engine_list;
static std::vector<SCIMInputMethod *> im_list;
static std::vector<SCIMContext *> context_list;
static std::vector<String> config_modules;
static std::vector<String> factories;

static ConfigModule *config_module = NULL;
static ConfigPointer config = NULL;

static BackEndPointer be = NULL;
static SocketClient panel;

static int instance_count = 0;

static void cb_commit( IMEngineInstanceBase *instance, const WideString &wstr );
static void cb_preedit_update( IMEngineInstanceBase *instance, const WideString &wstr, const AttributeList &attr );
static void cb_preedit_hide( IMEngineInstanceBase *instance );
static void cb_preedit_caret( IMEngineInstanceBase *instance, int caret );
static void cb_lookup_update( IMEngineInstanceBase *instance, const LookupTable &table );
static void cb_lookup_show( IMEngineInstanceBase *instance );
static void cb_lookup_hide( IMEngineInstanceBase *instance );

static const int UIM_PREEDIT_FLAG_UNDERLINE = 1;
static const int UIM_PREEDIT_FLAG_REVERSE   = 2;
static const int UIM_PREEDIT_FLAG_CURSOR    = 4;
static void uim_eval_im_commit(int id, const char *str);
static void uim_eval_im_clear_preedit(int id);
static void uim_eval_im_pushback_preedit(int id, int flag, const char *str);
static void uim_eval_im_update_preedit(int id);
static void uim_eval_im_activate_candidate_selector(int id, int nr_candidates, int display_limit);
static void uim_eval_im_select_candidate(int id, int index);
static void uim_eval_im_deactivate_candidate_selector(int id);

static void uim_keysymbol_to_scim_keysymbol( const char *sym, KeyEvent *key );

#define WideStr_to_CStr(widestr) ((char *)utf8_wcstombs(widestr).c_str())
#define WideStr_to_String(widestr) ((String)utf8_wcstombs(widestr))

static bool
check_socket_frontend()
{
    SocketAddress address;
    SocketClient client;
    uint32 magic;

    address.set_address (scim_get_default_socket_frontend_address ());

    if (!client.connect (address))
        return false;

    if (!scim_socket_open_connection (magic,
                                      String ("ConnectionTester"),
                                      String ("SocketFrontEnd"),
                                      client,
                                      1000))
    {
        return false;
    }
    return true;
}

static void
create_im_list()
{
    std::vector<IMEngineFactoryPointer> factories;

    int num = be->get_factories_for_language( factories );

    std::vector<IMEngineFactoryPointer>::iterator it = factories.begin();
    for ( ; it != factories.end(); ++it )
    {
        SCIMInputMethod *scim_im = new SCIMInputMethod();
	// FIXME: IMEngineFactoryBase::get_name() returns the IM name
	// for humans (and it is recommended to be 'localized'). So it
	// does not fit the uim's symbolic IM name (idname), and
	// causes parse errors on Scheme when being read as Scheme
	// symbol. For example, an IM name "scim-Probhat(phonetic)"
	// cannot be a safe Scheme symbol since it contains
	// parenthesis. Some character replacements and
	// anti-message-translation (localization) is required to
	// provide uim IMs. uim rejects IMs that have invalid IM
	// name.  -- YamaKen 2008-03-24
        scim_im->imname = (*it)->get_name();
        scim_im->lang = (*it)->get_language();
        scim_im->uuid = (*it)->get_uuid();

        im_list.push_back( scim_im );
    }
}


static uim_lisp
init_scim()
{
    fprintf( stderr, "init_scim()\n" );
    if ( !initialized )
    {
        context_list.clear();

        scim_get_imengine_module_list( engine_list );
        if ( std::find( engine_list.begin() , engine_list.end() , "socket") == engine_list.end() )
        {
            fprintf(stderr, "Could not find socket module.\n");
            return uim_scm_f();
        }

        config_module = new ConfigModule( "simple" );
        if ( !config_module )
        {
            uim_notify_fatal("init_scim: Could not create ConfigModule");
            return uim_scm_f();
        }

#if 0
	/* for old versions (1.2 or older) of SCIM */
        config = config_module->create_config("scim");
#else
	/* for new versions (1.3, 1.4) of SCIM */
        config = config_module->create_config();
#endif
        if ( config.null() )
        {
            uim_notify_fatal("init_scim: create_config failed");
            return uim_scm_f();
        }

        be = new CommonBackEnd( config, engine_list );
        if ( be.null() )
        {
            uim_notify_fatal("init_scim: create CommonBackEnd failed");
            return uim_scm_f();
        }

        if ( !check_socket_frontend() )
        {
            /*
             * 2004-03-03 Kazuki Ohta <mover@hct.zaq.ne.jp> @ChinaOSS CodeFest
             *
             * If no SocketFrontend is runnig, launch a SCIM daemon.
             */
            fprintf(stderr, "launch SCIM daemon\n");
            char *new_argv [] = { (char *)"--no-stay", 0 };
            scim_launch( true,
                         "simple", //FIX ME
                         (engine_list.size () ? scim_combine_string_list( engine_list, ',' ) : "all"),
                         "socket",
                         new_argv );

            return uim_scm_f();
        }

        create_im_list();

        initialized = 1;
    }

    return uim_scm_t();
}

static uim_lisp
get_nr_input_methods()
{
    return uim_scm_make_int( im_list.size() );
}

static uim_lisp
get_input_method_lang(uim_lisp nth_)
{
    // FIXME
    int nth = uim_scm_c_int( nth_ );
    if ( nth < (int)im_list.size() )
    {
        return uim_scm_make_str( im_list.at( nth )->lang.c_str() );
    }
    
    return uim_scm_f();
}

std::string& replace(std::string& str, const std::string sb, const std::string sa)
{
    std::string::size_type n, nb = 0;

    while ((n = str.find(sb,nb)) != std::string::npos)
    {
        str.replace(n,sb.size(),sa);
        nb = n + sa.size();
    }

    return str;
}

static uim_lisp
get_input_method_name(uim_lisp nth_)
{
    int nth = uim_scm_c_int( nth_ );
    if ( nth < (int)im_list.size() )
    {
        // remove space
        String imname = WideStr_to_String( im_list.at( nth )->imname );
        replace( imname, " ", "" );
        const char *orig_name = imname.c_str();

        // add "scim" as prefix
        char *name = (char *)alloca( strlen(orig_name) + 20 );
        if ( name )
        {
            sprintf(name, "scim-%s", orig_name);
            return uim_scm_make_str( name );
        }

        return uim_scm_f();
    }

    return uim_scm_f();
}

static String
search_uuid_by_imname( String imname )
{
    fprintf(stderr, "search imname = %s\n", imname.c_str() );

    
    std::vector<SCIMInputMethod *>::iterator it = im_list.begin();
    for ( ; it != im_list.end(); ++it )
    {
        fprintf(stderr, "im = %s\n", WideStr_to_CStr( (*it)->imname));

        // remove "scim-" prefix
        String name = imname.substr( 5, imname.length() - 5 );
        if ( WideStr_to_String((*it)->imname) == name )
        {
            return (*it)->uuid;
        }
    }

    return "";
}

static uim_lisp
alloc_id( uim_lisp name_ )
{
    char *imname = uim_scm_c_str(name_);
    fprintf(stderr, "scim.cpp : alloc_id = %s\n", imname);

    SCIMContext *context = new SCIMContext();

    // create factory by specifying the uuid
    String uuid = search_uuid_by_imname( imname );
    if ( uuid.empty() )
    {
        uim_notify_fatal( "uim-scim: failed to search uuid" );
        delete context;
        return uim_scm_f();
    }
    context->factory = be->get_factory( uuid );

    fprintf(stderr, "imname = %s\n", WideStr_to_CStr( context->factory->get_name() ));
    fprintf(stderr, "lang = %s\n", context->factory->get_language().c_str() );

    // initialize context's member variable
    context->instance = context->factory->create_instance( "UTF-8", instance_count );
    if ( context->instance.null() )
    {
        uim_notify_fatal("uim-scim: failed to create IMEngineInstance");
        delete context;
        return uim_scm_f();
    }

    context->id = instance_count;
    context->is_on = false;

    // set callbacks for the created instance
    context->instance->signal_connect_commit_string( slot( cb_commit) );
    context->instance->signal_connect_update_preedit_string( slot( cb_preedit_update) );
    context->instance->signal_connect_hide_preedit_string( slot( cb_preedit_hide) );
    context->instance->signal_connect_update_preedit_caret( slot( cb_preedit_caret) );
    context->instance->signal_connect_update_lookup_table( slot( cb_lookup_update) );
    context->instance->signal_connect_show_lookup_table( slot( cb_lookup_show) );
    context->instance->signal_connect_hide_lookup_table( slot( cb_lookup_hide) );
    /*
    context->instance->signal_connect_register_properties( slot( cb_prop_register) );
    context->instance->signal_connect_update_property( slot( cb_prop_update) );
    */
    context->instance->set_frontend_data( static_cast <void *> (context) );

    // and store the context
    context_list.push_back( context );

    instance_count++;

    free(imname);

    return uim_scm_make_int( context->id );
}

static uim_lisp
free_id(uim_lisp id_)
{
    return uim_scm_f();
}

static SCIMContext *get_context_from_id(int id)
{
    std::vector<SCIMContext *>::iterator it = context_list.begin();
    for ( ; it != context_list.end(); ++it )
    {
        if ( id == (*it)->id )
        {
            return (*it);
        }
    }

    return NULL;
}

static int
ukey_mod_to_skey_mod(int mod)
{
  int rv = 0;
  if (mod & UMod_Shift) {
    rv |= SCIM_KEY_ShiftMask;
  }
  if (mod & UMod_Control) {
    rv |= SCIM_KEY_ControlMask;
  }
  if (mod & UMod_Alt) {
    rv |= SCIM_KEY_AltMask;
  }
  if (mod & UMod_Super) {  /* assuming mod3 */
    rv |= SCIM_KEY_Mod3Mask;
  }
  if (mod & UMod_Hyper) {  /* assuming mod4 */
    rv |= SCIM_KEY_Mod3Mask;
  }
  return rv;
}

static uim_lisp
push_key(uim_lisp id_, uim_lisp key_, uim_lisp mod_)
{
    fprintf(stderr, "push_key\n");
    int id = uim_scm_c_int( id_ );
    int code = uim_scm_c_int( key_ );
    int mod = uim_scm_c_int( mod_ );

    KeyEvent scim_key;
    scim_key.code = code;
    scim_key.mask = ukey_mod_to_skey_mod(mod);

    SCIMContext *ic = get_context_from_id( id );
    if ( ic->instance->process_key_event( scim_key ) )
    {
        return uim_scm_t();
    }

    return uim_scm_f();
}

static uim_lisp
push_symbol_key(uim_lisp id_, uim_lisp key_, uim_lisp mod_)
{
    int id = uim_scm_c_int(id_);
    int mod = uim_scm_c_int(mod_);
    const char *sym = uim_scm_refer_c_str(key_);
    fprintf(stderr, "push_symbol_key = %s\n", sym);

    KeyEvent scim_key;
    uim_keysymbol_to_scim_keysymbol(sym, &scim_key);
    scim_key.mask = ukey_mod_to_skey_mod(mod);

    SCIMContext *ic = get_context_from_id( id );
    if (ic->instance->process_key_event( scim_key )) {
      return uim_scm_t();
    }

    return uim_scm_f();
}

static uim_lisp
get_nth_candidate( uim_lisp id_, uim_lisp idx_ )
{
    int id = uim_scm_c_int( id_ );
    int idx = uim_scm_c_int( idx_ );

    SCIMContext *ic = get_context_from_id( id );
    if ( !ic )
    {
        return uim_scm_f();
    }

    return uim_scm_f();
}

static void cb_commit( IMEngineInstanceBase *instance, const WideString &wstr )
{
    fprintf(stderr, "cb_commit\n");
    SCIMContext *ic = static_cast<SCIMContext *>(instance->get_frontend_data());
    if ( !ic )
    {
        return;
    }

    ic->preedit_str = WideString();
    ic->preedit_attr.clear();
    ic->preedit_caret = 0;

    uim_eval_im_clear_preedit( ic->id );
    uim_eval_im_update_preedit( ic->id );
    uim_eval_im_commit( ic->id, WideStr_to_CStr( wstr ) );
}

static void cb_preedit_update( IMEngineInstanceBase *instance, const WideString &wstr, const AttributeList &attr )
{    
    fprintf(stderr, "cb_preedit_update : preedit_str = [%s]\n", WideStr_to_CStr(wstr));

    SCIMContext *ic = static_cast<SCIMContext *>(instance->get_frontend_data());
    if ( !ic )
    {
        return;
    }

    ic->preedit_str = wstr;
    ic->preedit_attr = attr;

    uim_eval_im_clear_preedit( ic->id );
    uim_eval_im_pushback_preedit( ic->id, 0, WideStr_to_CStr(wstr) );
    uim_eval_im_update_preedit( ic->id );
}
static void cb_preedit_hide( IMEngineInstanceBase *instance )
{
    fprintf(stderr, "cb_preedit_hide\n");

    SCIMContext *ic = static_cast<SCIMContext *>(instance->get_frontend_data());
    if ( !ic )
    {
        return;
    }

    ic->preedit_str = WideString();
    ic->preedit_attr.clear();

    uim_eval_im_update_preedit( ic->id );
}
static void cb_preedit_caret( IMEngineInstanceBase *instance, int caret )
{
    fprintf(stderr, "cb_preedit_caret\n");

    SCIMContext *ic = static_cast<SCIMContext *>(instance->get_frontend_data());
    if ( !ic )
    {
        return;
    }

    // FIXME
    // makes nothing in present state
    ic->preedit_caret = caret;

    uim_eval_im_update_preedit( ic->id );
}
static void cb_lookup_update( IMEngineInstanceBase *instance, const LookupTable &table )
{
    fprintf(stderr, "cb_lookup_update\n");

    SCIMContext *ic = static_cast<SCIMContext *>(instance->get_frontend_data());
    if ( !ic )
    {
        return;
    }

}
static void cb_lookup_show( IMEngineInstanceBase *instance )
{
    fprintf(stderr, "cb_lookup_show\n");
}
static void cb_lookup_hide( IMEngineInstanceBase *instance )
{
    fprintf(stderr, "cb_lookup_hide\n");
}

static void
uim_eval_im_commit(int id, const char *str)
{
  uim_lisp form = uim_scm_list3(uim_scm_make_symbol("im-commit"),
				uim_scm_make_int(id),
				uim_scm_make_str(str));
  uim_scm_eval(form);
}

static void
uim_eval_im_clear_preedit(int id)
{
  uim_lisp form = uim_scm_list2(uim_scm_make_symbol("im-clear-preedit"),
				uim_scm_make_int(id));
  uim_scm_eval(form);
}

static void
uim_eval_im_pushback_preedit(int id, int flag, const char *str)
{
  uim_lisp form;
  /*
   * FIXME! : 2004-02-12 Kazuki Ohta <mover@hct.zaq.ne.jp>
   * This is very adhoc hack!
   * Consider multiple attr!
   */
  if (flag & UIM_PREEDIT_FLAG_CURSOR) {
    form = uim_scm_list4(uim_scm_make_symbol("im-pushback-preedit"),
			 uim_scm_make_int(id),
			 uim_scm_make_symbol("preedit-cursor"),
			 uim_scm_make_str(""));
    uim_scm_eval(form);
  } else if (flag & UIM_PREEDIT_FLAG_REVERSE) {
    form = uim_scm_list4(uim_scm_make_symbol("im-pushback-preedit"),
			 uim_scm_make_int(id),
			 uim_scm_make_symbol("preedit-reverse"),
			 uim_scm_make_str(str));
    uim_scm_eval(form);
  } else {
    form = uim_scm_list4(uim_scm_make_symbol("im-pushback-preedit"),
			 uim_scm_make_int(id),
			 uim_scm_make_symbol("preedit-underline"),
			 uim_scm_make_str(str));
    uim_scm_eval(form);
  }

}

static void
uim_eval_im_update_preedit(int id)
{
  uim_lisp form = uim_scm_list2(uim_scm_make_symbol("im-update-preedit"),
				uim_scm_make_int(id));
  uim_scm_eval(form);
}

static void uim_eval_im_activate_candidate_selector(int id, int nr_candidates, int display_limit)
{
  uim_lisp form = uim_scm_list4(uim_scm_make_symbol("im-activate-candidate-selector"),
				uim_scm_make_int(id),
				uim_scm_make_int(nr_candidates),
				uim_scm_make_int(display_limit));
  uim_scm_eval(form);
}

static void uim_eval_im_select_candidate(int id, int index)
{
  uim_lisp form = uim_scm_list3(uim_scm_make_symbol("im-select-candidate"),
				uim_scm_make_int(id),
				uim_scm_make_int(index));
  uim_scm_eval(form);
}

static void uim_eval_im_deactivate_candidate_selector(int id)
{
  uim_lisp form = uim_scm_list2(uim_scm_make_symbol("im-deactivate-candidate-selector"),
				uim_scm_make_int(id));
  uim_scm_eval(form);
}


extern "C" void
uim_plugin_instance_init(void)
{
    uim_scm_init_subr_0("scim-lib-init", init_scim);
    uim_scm_init_subr_0("scim-lib-nr-input-methods", get_nr_input_methods);
    uim_scm_init_subr_1("scim-lib-nth-input-method-lang", get_input_method_lang);
    uim_scm_init_subr_1("scim-lib-nth-input-method-name", get_input_method_name);
    uim_scm_init_subr_1("scim-lib-alloc-context", alloc_id);
    uim_scm_init_subr_1("scim-lib-free-context", free_id);
    uim_scm_init_subr_3("scim-lib-push-key", push_key);
    uim_scm_init_subr_3("scim-lib-push-symbol-key", push_symbol_key);
    uim_scm_init_subr_2("sicm-lib-nth-candidate", get_nth_candidate);
}

extern "C" void
uim_plugin_instance_quit(void)
{

}

static void
uim_keysymbol_to_scim_keysymbol( const char *sym, KeyEvent *key )
{
    fprintf(stderr, "uim_keysymbol! : sym = %s\n", sym);
    static struct keycode_map_ {
        char *symbol;
        unsigned int keycode;
    } keycode_map[] = {
        {(char *)"Escape", SCIM_KEY_Escape},
        {(char *)"Tab", SCIM_KEY_Tab},
        {(char *)"BackSpace", SCIM_KEY_BackSpace},
        {(char *)"Delete", SCIM_KEY_Delete},
        {(char *)"Return", SCIM_KEY_Return},
        {(char *)"Left", SCIM_KEY_Left},
        {(char *)"Up", SCIM_KEY_Up},
        {(char *)"Right", SCIM_KEY_Right},
        {(char *)"Down", SCIM_KEY_Down},
        {(char *)"Prior", SCIM_KEY_Prior},
        {(char *)"Next", SCIM_KEY_Next},
        {(char *)"Home", SCIM_KEY_Home},
        {(char *)"End", SCIM_KEY_End},
        {NULL, 0}
    };

    struct keycode_map_ *l;
    for ( l = keycode_map; l->symbol; l++ )
    {
        if ( strcmp(sym, l->symbol) == 0 )
        {
            fprintf(stderr, "keysymbol = %s\n", l->symbol);
            (*key).code = l->keycode;
            return;
        }
    }

}
