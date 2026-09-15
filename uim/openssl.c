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
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/x509.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-notify.h"
#include "gettext.h"
#include "dynlib.h"

static const char *
openssl_error_string(void)
{
  static char buf[256];

  ERR_error_string_n(ERR_get_error(), buf, sizeof(buf));
  return buf;
}

/*
 * (openssl-client-connect fd hostname) => ssl or #f
 *
 * Wraps the already connected socket FD with TLS as a client.  The
 * peer certificate is verified against the system certificate store
 * and HOSTNAME.  HOSTNAME is also sent as SNI.
 *
 * The returned SSL object owns its SSL_CTX; SSL-free releases both.
 */
static uim_lisp
c_openssl_client_connect(uim_lisp fd_, uim_lisp hostname_)
{
  const char *hostname = REFER_C_STR(hostname_);
  int fd = C_INT(fd_);
  SSL_CTX *ctx;
  SSL *ssl;
  long verify_result;

  ctx = SSL_CTX_new(TLS_client_method());
  if (!ctx) {
    uim_notify_fatal(N_("uim-openssl: SSL_CTX_new: %s"), openssl_error_string());
    return uim_scm_f();
  }

  SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
  SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
  if (!SSL_CTX_set_default_verify_paths(ctx)) {
    uim_notify_fatal(N_("uim-openssl: SSL_CTX_set_default_verify_paths: %s"),
                     openssl_error_string());
    SSL_CTX_free(ctx);
    return uim_scm_f();
  }

  ssl = SSL_new(ctx);
  /* SSL_new() took its own reference; the SSL object now owns the context. */
  SSL_CTX_free(ctx);
  if (!ssl) {
    uim_notify_fatal(N_("uim-openssl: SSL_new: %s"), openssl_error_string());
    return uim_scm_f();
  }

  if (!SSL_set_tlsext_host_name(ssl, hostname)
      || !X509_VERIFY_PARAM_set1_host(SSL_get0_param(ssl), hostname, 0)
      || !SSL_set_fd(ssl, fd)) {
    uim_notify_fatal(N_("uim-openssl: cannot set up connection to %s: %s"),
                     hostname, openssl_error_string());
    SSL_free(ssl);
    return uim_scm_f();
  }

  if (SSL_connect(ssl) != 1) {
    verify_result = SSL_get_verify_result(ssl);
    if (verify_result != X509_V_OK)
      uim_notify_fatal(N_("uim-openssl: certificate verification failed for %s: %s"),
                       hostname, X509_verify_cert_error_string(verify_result));
    else
      uim_notify_fatal(N_("uim-openssl: SSL_connect to %s: %s"),
                       hostname, openssl_error_string());
    SSL_free(ssl);
    return uim_scm_f();
  }

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
  nr = SSL_read(C_PTR(s_), buf, nbytes);
  if (nr <= 0) {
    free(buf);
    return (nr == 0) ? uim_scm_eof() : uim_scm_f();
  }

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

void
uim_plugin_instance_init(void)
{
  /* OpenSSL >= 1.1.0 initializes itself on first use. */
  uim_scm_init_proc2("openssl-client-connect", c_openssl_client_connect);
  uim_scm_init_proc1("SSL-free", c_SSL_free);
  uim_scm_init_proc1("SSL-get-version", c_SSL_get_version);
  uim_scm_init_proc1("SSL-get-cipher", c_SSL_get_cipher);
  uim_scm_init_proc1("SSL-shutdown", c_SSL_shutdown);
  uim_scm_init_proc2("SSL-read", c_SSL_read);
  uim_scm_init_proc2("SSL-write", c_SSL_write);
}

void
uim_plugin_instance_quit(void)
{
}
