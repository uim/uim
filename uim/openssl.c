/*

  Copyright (c) 2009-2013 uim Project https://github.com/uim/uim

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
#include <string.h>
#include <openssl/crypto.h>
#include <openssl/ssl.h>
#include <openssl/rand.h>
#include <openssl/err.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-posix.h"
#include "uim-notify.h"
#include "gettext.h"
#include "dynlib.h"

#ifdef USE_OPENSSL_ENGINE
# include <openssl/engine.h>
#endif

static uim_lisp
c_ERR_get_error(void)
{
  return MAKE_INT(ERR_get_error());
}

static uim_lisp
c_ERR_error_string(uim_lisp e_)
{
  char buf[BUFSIZ];

  /* XXX: long->int */
  ERR_error_string_n(C_INT(e_), buf, sizeof(buf));
  return MAKE_STR(buf);
}

static uim_lisp
c_SSL_CTX_new(uim_lisp meth_)
{
  SSL_CTX *ctx = SSL_CTX_new(C_PTR(meth_));

  if (!ctx)
    return uim_scm_f();
  return MAKE_PTR(ctx);
}

static uim_lisp
c_SSL_CTX_free(uim_lisp ctx_)
{
  SSL_CTX_free(C_PTR(ctx_));
  return uim_scm_t();
}


static uim_lisp
c_SSL_new(uim_lisp ctx_)
{
  SSL *ssl = SSL_new(C_PTR(ctx_));

  if (!ssl)
    return uim_scm_f();

  return MAKE_PTR(ssl);
}

static uim_lisp
c_SSL_free(uim_lisp s_)
{
  SSL_free(C_PTR(s_));
  return uim_scm_t();
}

static uim_lisp
c_SSL_get_version(uim_lisp s_)
{
  return MAKE_STR(SSL_get_version(C_PTR(s_)));
}


static uim_lisp
c_SSL_get_cipher(uim_lisp s_)
{
  return MAKE_STR(SSL_get_cipher(C_PTR(s_)));
}


static uim_lisp
c_SSL_shutdown(uim_lisp s_)
{
  return MAKE_INT(SSL_shutdown(C_PTR(s_)));
}

static uim_lisp
c_SSL_set_fd(uim_lisp s_, uim_lisp fd_)
{
  return MAKE_INT(SSL_set_fd(C_PTR(s_), C_INT(fd_)));
}

static uim_lisp
c_SSL_connect(uim_lisp s_)
{

  RAND_poll();
  srand(time(NULL));

  while (RAND_status() == 0) {
    unsigned short seed = (unsigned short)rand();

    RAND_seed(&seed, sizeof(seed));
  }
  return MAKE_INT(SSL_connect(C_PTR(s_)));
}

struct c_SSL_read_args {
  const unsigned char *buf;
  int nr;
};

static uim_lisp
c_SSL_read_internal(struct c_SSL_read_args *args)
{
  int i;
  uim_lisp ret_ = uim_scm_null();
  const unsigned char *p = args->buf;

  for (i = 0; i < args->nr; i++) {
    ret_ = CONS(MAKE_CHAR(*p), ret_);
    p++;
  }
  return ret_;
}

static uim_lisp
c_SSL_read(uim_lisp s_, uim_lisp nbytes_)
{
  unsigned char *buf;
  uim_lisp ret_;
  int nbytes = C_INT(nbytes_);
  int nr;
  struct c_SSL_read_args args;

  buf = uim_malloc(nbytes);
  if ((nr = SSL_read(C_PTR(s_), buf, nbytes)) == 0)
    return uim_scm_eof();
  if (nr < 0)
    return uim_scm_f();

  args.buf = buf;
  args.nr = nr;
  ret_ = (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)c_SSL_read_internal,
						    (void *)&args);
  free(buf);
  return uim_scm_callf("reverse", "o", ret_);
}

static uim_lisp
c_SSL_write(uim_lisp s_, uim_lisp buf_)
{
  int nbytes = uim_scm_length(buf_);
  uim_lisp ret_;
  unsigned char *buf;
  unsigned char *p;

  buf = p = uim_malloc(nbytes);
  while (!NULLP(buf_)) {
    *p = C_CHAR(CAR(buf_));
    p++;
    buf_ = CDR(buf_);
  }
  ret_ = MAKE_INT((int)SSL_write(C_PTR(s_), buf, nbytes));
  free(buf);
  return ret_;
}

/* SSLv2 */
static uim_lisp
c_SSLv2_method(void)
{
  return MAKE_PTR(SSLv2_method());
}
static uim_lisp
c_SSLv2_server_method(void)
{
  return MAKE_PTR(SSLv2_server_method());
}
static uim_lisp
c_SSLv2_client_method(void)
{
  return MAKE_PTR(SSLv2_client_method());
}

/* SSLv3 */
static uim_lisp
c_SSLv3_method(void)
{
  return MAKE_PTR(SSLv3_method());
}
static uim_lisp
c_SSLv3_server_method(void)
{
  return MAKE_PTR(SSLv3_server_method());
}
static uim_lisp
c_SSLv3_client_method(void)
{
  return MAKE_PTR(SSLv3_client_method());
}

/* SSLv3 but can rollback to v2 */
static uim_lisp
c_SSLv23_method(void)
{
  return MAKE_PTR(SSLv23_method());
}
static uim_lisp
c_SSLv23_server_method(void)
{
  return MAKE_PTR(SSLv23_server_method());
}
static uim_lisp
c_SSLv23_client_method(void)
{
  return MAKE_PTR(SSLv23_client_method());
}

/* TLSv1.0 */
static uim_lisp
c_TLSv1_method(void)
{
  return MAKE_PTR(TLSv1_method());
}
static uim_lisp
c_TLSv1_server_method(void)
{
  return MAKE_PTR(TLSv1_server_method());
}
static uim_lisp
c_TLSv1_client_method(void)
{
  return MAKE_PTR(TLSv1_client_method());
}

/* DTLSv1.0 */
static uim_lisp
c_DTLSv1_method(void)
{
#ifdef HAVE_OPENSSL_DTLSv1
  return MAKE_PTR(DTLSv1_method());
#else
  uim_notify_fatal(N_("uim-openssl: DTLSv1_method() is not supported on this system"));
  return uim_scm_f();
#endif
}
static uim_lisp
c_DTLSv1_server_method(void)
{
#ifdef HAVE_OPENSSL_DTLSv1
  return MAKE_PTR(DTLSv1_server_method());
#else
  uim_notify_fatal(N_("uim-openssl: DTLSv1_server_method() is not supported on this system"));
  return uim_scm_f();
#endif
}
static uim_lisp
c_DTLSv1_client_method(void)
{
#ifdef HAVE_OPENSSL_DTLSv1
  return MAKE_PTR(DTLSv1_client_method());
#else
  uim_notify_fatal(N_("uim-openssl: DTLSv1_client_method() is not supported on this system"));
  return uim_scm_f();
#endif
}


void
uim_plugin_instance_init(void)
{
  /* too old? */
  if (!SSLeay_add_ssl_algorithms())
    return;
  if (!SSL_library_init())
    return;
#ifdef  USE_OPENSSL_ENGINE
  ENGINE_load_builtin_engines();
  ENGINE_register_all_complete();
#endif

  SSL_load_error_strings();

  uim_scm_init_proc0("ERR-get-error", c_ERR_get_error);
  uim_scm_init_proc1("ERR-error-string", c_ERR_error_string);
  uim_scm_init_proc1("SSL-CTX-new", c_SSL_CTX_new);
  uim_scm_init_proc1("SSL-CTX-free", c_SSL_CTX_free);
  uim_scm_init_proc1("SSL-new", c_SSL_new);
  uim_scm_init_proc1("SSL-free", c_SSL_free);
  uim_scm_init_proc1("SSL-get-version", c_SSL_get_version);
  uim_scm_init_proc1("SSL-get-cipher", c_SSL_get_cipher);
  uim_scm_init_proc1("SSL-shutdown", c_SSL_shutdown);
  uim_scm_init_proc2("SSL-set-fd", c_SSL_set_fd);
  uim_scm_init_proc1("SSL-connect", c_SSL_connect);
  uim_scm_init_proc2("SSL-read", c_SSL_read);
  uim_scm_init_proc2("SSL-write", c_SSL_write);
  uim_scm_init_proc0("SSLv2-method", c_SSLv2_method);
  uim_scm_init_proc0("SSLv2-server-method", c_SSLv2_server_method);
  uim_scm_init_proc0("SSLv2-client-method", c_SSLv2_client_method);
  uim_scm_init_proc0("SSLv3-method", c_SSLv3_method);
  uim_scm_init_proc0("SSLv3-server-method", c_SSLv3_server_method);
  uim_scm_init_proc0("SSLv3-client-method", c_SSLv3_client_method);
  uim_scm_init_proc0("SSLv23-method", c_SSLv23_method);
  uim_scm_init_proc0("SSLv23-server-method", c_SSLv23_server_method);
  uim_scm_init_proc0("SSLv23-client-method", c_SSLv23_client_method);
  uim_scm_init_proc0("TLSv1-method", c_TLSv1_method);
  uim_scm_init_proc0("TLSv1-server-method", c_TLSv1_server_method);
  uim_scm_init_proc0("TLSv1-client-method", c_TLSv1_client_method);
  uim_scm_init_proc0("DTLSv1-method", c_DTLSv1_method);
  uim_scm_init_proc0("DTLSv1-server-metho", c_DTLSv1_server_method);
  uim_scm_init_proc0("DTLSv1-client-method", c_DTLSv1_client_method);
}

void
uim_plugin_instance_quit(void)
{
  ERR_free_strings();
}
