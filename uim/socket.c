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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/param.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/un.h>

#include "uim.h"
#include "uim-internal.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-posix.h"
#include "uim-notify.h"
#include "gettext.h"
#include "dynlib.h"

typedef struct {
  int flag;
  char *arg;
} opt_args;

static uim_lisp
make_arg_cons(const opt_args *arg)
{
  return CONS(MAKE_SYM(arg->arg), MAKE_INT(arg->flag));
}

static uim_lisp
make_arg_list(const opt_args *list)
{
  uim_lisp ret_;
  int i = 0;

  ret_ = uim_scm_null();
  while (list[i].arg != 0) {
    ret_ = CONS((uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)make_arg_cons,
							   (void *)&list[i]), ret_);
    i++;
  }
  return ret_;
}

static uim_lisp
c_make_addrinfo()
{
  struct addrinfo *addrinfo = uim_malloc(sizeof(struct addrinfo));

  memset(addrinfo, 0, sizeof(struct addrinfo));
  return MAKE_PTR(addrinfo);
}

static uim_lisp
c_delete_addrinfo(uim_lisp addrinfo_)
{
  free(C_PTR(addrinfo_));
  return uim_scm_t();
}

const static opt_args ai_flags[] = {
  { AI_CANONNAME,   "$AI_CANONNAME" },
  { AI_NUMERICHOST, "$AI_NUMERICHOST" },
  { AI_PASSIVE,     "$AI_PASSIVE" },
  { 0, 0 }
};
static uim_lisp uim_lisp_ai_flags;
static uim_lisp
c_addrinfo_ref_ai_flags_alist(void)
{
  return uim_lisp_ai_flags;
}

static uim_lisp
c_addrinfo_set_ai_flags(uim_lisp addrinfo_, uim_lisp ai_flags_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);

  addrinfo->ai_flags = C_INT(ai_flags_);
  return uim_scm_t();
}
static uim_lisp
c_addrinfo_ref_ai_flags(uim_lisp addrinfo_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);

  return MAKE_INT(addrinfo->ai_flags);
}

const static opt_args ai_family[] = {
  { PF_UNSPEC, "$PF_UNSPEC" },
#ifndef PF_LOCAL
  { PF_UNIX,   "$PF_LOCAL" },
#else
  { PF_LOCAL,  "$PF_LOCAL" },
#endif
  { PF_INET,   "$PF_INET" },
  { PF_INET6,  "$PF_INET6" },
  { 0, 0 }
};

static uim_lisp uim_lisp_ai_family;
static uim_lisp
c_addrinfo_ref_ai_family_alist(void)
{
  return uim_lisp_ai_family;
}
static uim_lisp
c_addrinfo_set_ai_family(uim_lisp addrinfo_, uim_lisp ai_family_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);

  addrinfo->ai_family = C_INT(ai_family_);
  return uim_scm_t();
}
static uim_lisp
c_addrinfo_ref_ai_family(uim_lisp addrinfo_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);

  return MAKE_INT(addrinfo->ai_family);
}

const static opt_args ai_socktype[] = {
  { SOCK_STREAM, "$SOCK_STREAM" },
  { SOCK_DGRAM,  "$SOCK_DGRAM" },
  { SOCK_RAW,    "$SOCK_RAW" },
  { 0, 0 }
};

static uim_lisp uim_lisp_ai_socktype;
static uim_lisp
c_addrinfo_ref_ai_socktype_alist(void)
{
  return uim_lisp_ai_socktype;
}
static uim_lisp
c_addrinfo_set_ai_socktype(uim_lisp addrinfo_, uim_lisp ai_socktype_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);

  addrinfo->ai_socktype = C_INT(ai_socktype_);
  return uim_scm_t();
}
static uim_lisp
c_addrinfo_ref_ai_socktype(uim_lisp addrinfo_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);

  return MAKE_INT(addrinfo->ai_socktype);
}

const static opt_args ai_protocol[] = {
  { IPPROTO_UDP, "$IPPROTO_UDP" },
  { IPPROTO_TCP, "$IPPROTO_TCP" },
  { 0, 0 }
};

static uim_lisp uim_lisp_ai_protocol;
static uim_lisp
c_addrinfo_ref_ai_protocol_alist(void)
{
  return uim_lisp_ai_protocol;
}
static uim_lisp
c_addrinfo_set_ai_protocol(uim_lisp addrinfo_, uim_lisp ai_protocol_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);

  addrinfo->ai_protocol = C_INT(ai_protocol_);
  return uim_scm_t();
}
static uim_lisp
c_addrinfo_ref_ai_protocol(uim_lisp addrinfo_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);

  return MAKE_INT(addrinfo->ai_protocol);
}

static uim_lisp
c_addrinfo_ref_ai_addrlen(uim_lisp addrinfo_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);

  return MAKE_INT(addrinfo->ai_addrlen);
}

static uim_lisp
c_addrinfo_ref_ai_addr(uim_lisp addrinfo_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);

  return MAKE_PTR(addrinfo->ai_addr);
}

static uim_lisp
c_getaddrinfo(uim_lisp hostname_, uim_lisp servname_, uim_lisp hint_)
{
  const char *hostname;
  char *servname = NULL;
  struct addrinfo *hints = C_PTR(hint_);
  struct addrinfo *res, *res0;
  uim_lisp ret_ = uim_scm_null();
  int error;

  if (INTP(servname_)) {
    uim_asprintf(&servname, "%d", C_INT(servname_));
  } else {
    servname = C_STR(servname_);
  }

  if (FALSEP(hostname_))
    hostname = NULL;
  else
    hostname = REFER_C_STR(hostname_);
  error = getaddrinfo(hostname, servname, hints, &res0);
  if (error) {
    const char *errstr = gai_strerror(error);
    uim_notify_fatal("getaddrinfo: %s", errstr);
    free(servname);
    return uim_scm_f();
  }

  free(servname);
  for (res = res0; res; res = res->ai_next) {
    ret_ = CONS(MAKE_PTR(res) , ret_);
  }
  return uim_scm_callf("reverse", "o", ret_);
}


static uim_lisp
c_freeaddrinfo(uim_lisp addrinfo_)
{
  freeaddrinfo(C_PTR(addrinfo_));
  return uim_scm_t();
}

static uim_lisp
c_socket(uim_lisp domain_, uim_lisp type_, uim_lisp protocol_)
{
  int type_i = C_INT(type_);
  int fd;
#ifdef SOCK_CLOEXEC
  /* linux-2.6.27+ variant that prevents racing on concurrent fork & exec in other thread */
  fd = socket(C_INT(domain_), type_i | SOCK_CLOEXEC, C_INT(protocol_));
  if (fd == -1 && errno == EINVAL)
    /* fallback to plain SOCK_TYPE on older kernel */
#endif
    fd = socket(C_INT(domain_), type_i, C_INT(protocol_));
  if (fd != -1)
    fcntl(fd, F_SETFD, fcntl(fd, F_GETFD, 0) | FD_CLOEXEC);
  return MAKE_INT(fd);
}

static uim_lisp
c_connect(uim_lisp s_, uim_lisp name_, uim_lisp namelen_)
{
  return MAKE_INT(connect(C_INT(s_), C_PTR(name_), C_INT(namelen_)));
}

static uim_lisp
c_bind(uim_lisp s_, uim_lisp name_, uim_lisp namelen_)
{
  return MAKE_INT(bind(C_INT(s_), C_PTR(name_), C_INT(namelen_)));
}

static uim_lisp
c_listen(uim_lisp s_, uim_lisp backlog_)
{
  return MAKE_INT(listen(C_INT(s_), C_INT(backlog_)));
}

const static opt_args shutdown_how[] = {
  { SHUT_RD,   "$SHUT_RD" },
  { SHUT_WR,   "$SHUT_WR" },
  { SHUT_RDWR, "$SHUT_RDWR" },
  { 0, 0 }
};
static uim_lisp uim_lisp_shutdown_how_alist;
static uim_lisp
c_shutdown_how_alist(void)
{
  return uim_lisp_shutdown_how_alist;
}
static uim_lisp
c_shutdown(uim_lisp s_, uim_lisp how_)
{
  return MAKE_INT(shutdown(C_INT(s_), C_INT(how_)));
}

static uim_lisp
c_make_sockaddr_storage(void)
{
  return MAKE_PTR(uim_malloc(sizeof(struct sockaddr_storage)));
}

static uim_lisp
c_delete_sockaddr_storage(uim_lisp storage_)
{
  free(C_PTR(storage_));
  return uim_scm_t();
}

static uim_lisp
c_accept(uim_lisp s_, uim_lisp storage_)
{
  socklen_t storagelen;
  struct sockaddr_storage *storage = C_PTR(storage_);

  storagelen = sizeof(struct sockaddr_storage);
  return MAKE_INT(accept(C_INT(s_), (struct sockaddr *)storage, &storagelen));
}

static uim_lisp
c_make_sockaddr_un(void)
{
  struct sockaddr_un *s_un;

  s_un = uim_malloc(sizeof(struct sockaddr_un));
  memset(s_un, 0, sizeof(struct sockaddr_un));
  return MAKE_PTR(s_un);
}

static uim_lisp
c_delete_sockaddr_un(uim_lisp sun_)
{
  struct sockaddr_un *s_un = C_PTR(sun_);

  free(s_un);
  return uim_scm_t();
}

static uim_lisp
c_set_sockaddr_un_sun_family(uim_lisp sun_, uim_lisp family_)
{
  struct sockaddr_un *s_un = C_PTR(sun_);

  s_un->sun_family = C_INT(family_);
  return uim_scm_t();
}
static uim_lisp
c_ref_sockaddr_un_sun_family(uim_lisp sun_)
{
  struct sockaddr_un *s_un = C_PTR(sun_);

  return MAKE_INT(s_un->sun_family);
}

static uim_lisp
c_set_sockaddr_un_sun_path(uim_lisp sun_, uim_lisp path_)
{
  struct sockaddr_un *s_un = C_PTR(sun_);

  strlcpy(s_un->sun_path, REFER_C_STR(path_), sizeof(s_un->sun_path));
  return uim_scm_t();
}
static uim_lisp
c_ref_sockaddr_un_sun_path(uim_lisp sun_)
{
  struct sockaddr_un *s_un = C_PTR(sun_);

  return MAKE_STR(s_un->sun_path);
}

#ifndef SUN_LEN
#define SUN_LEN(su) \
  (sizeof(*(su)) - sizeof((su)->sun_path) + strlen((su)->sun_path))
#endif

static uim_lisp
c_sun_len(uim_lisp sun_)
{
  struct sockaddr_un *s_un = C_PTR(sun_);

  return MAKE_INT(SUN_LEN(s_un));
}

static uim_lisp
c_getpeereid(uim_lisp s_)
{
  uid_t euid;
  gid_t egid;

  if (getpeereid(C_INT(s_), &euid, &egid) == -1)
    return uim_scm_f();
  return CONS(MAKE_INT(euid), MAKE_INT(egid));
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc0("make-addrinfo", c_make_addrinfo);
  uim_scm_init_proc1("delete-addrinfo", c_delete_addrinfo);

  uim_scm_init_proc2("addrinfo-set-ai-flags!", c_addrinfo_set_ai_flags);
  uim_scm_init_proc1("addrinfo-ai-flags?", c_addrinfo_ref_ai_flags);
  uim_scm_init_proc0("addrinfo-ai-flags-alist?", c_addrinfo_ref_ai_flags_alist);
  uim_lisp_ai_flags = make_arg_list(ai_flags);
  uim_scm_gc_protect(&uim_lisp_ai_flags);

  uim_scm_init_proc2("addrinfo-set-ai-family!", c_addrinfo_set_ai_family);
  uim_scm_init_proc1("addrinfo-ai-family?", c_addrinfo_ref_ai_family);
  uim_scm_init_proc0("addrinfo-ai-family-alist?", c_addrinfo_ref_ai_family_alist);
  uim_lisp_ai_family = make_arg_list(ai_family);
  uim_scm_gc_protect(&uim_lisp_ai_family);

  uim_scm_init_proc2("addrinfo-set-ai-socktype!", c_addrinfo_set_ai_socktype);
  uim_scm_init_proc1("addrinfo-ai-socktype?", c_addrinfo_ref_ai_socktype);
  uim_scm_init_proc0("addrinfo-ai-socktype-alist?", c_addrinfo_ref_ai_socktype_alist);
  uim_lisp_ai_socktype = make_arg_list(ai_socktype);
  uim_scm_gc_protect(&uim_lisp_ai_socktype);

  uim_scm_init_proc2("addrinfo-set-ai-protocol!", c_addrinfo_set_ai_protocol);
  uim_scm_init_proc1("addrinfo-ai-protocol?", c_addrinfo_ref_ai_protocol);
  uim_scm_init_proc0("addrinfo-ai-protocol-alist?", c_addrinfo_ref_ai_protocol_alist);
  uim_lisp_ai_protocol = make_arg_list(ai_protocol);
  uim_scm_gc_protect(&uim_lisp_ai_protocol);

  uim_scm_init_proc1("addrinfo-ai-addrlen?", c_addrinfo_ref_ai_addrlen);
  uim_scm_init_proc1("addrinfo-ai-addr?", c_addrinfo_ref_ai_addr);

  uim_scm_init_proc0("make-sockaddr-un", c_make_sockaddr_un);
  uim_scm_init_proc1("delete-sockaddr-un", c_delete_sockaddr_un);

  uim_scm_init_proc2("sockaddr-set-un-sun-family!", c_set_sockaddr_un_sun_family);
  uim_scm_init_proc1("sockaddr-un-sun-family?", c_ref_sockaddr_un_sun_family);

  uim_scm_init_proc2("sockaddr-set-un-sun-path!", c_set_sockaddr_un_sun_path);
  uim_scm_init_proc1("sockaddr-un-sun-path?", c_ref_sockaddr_un_sun_path);

  uim_scm_init_proc1("sun-len", c_sun_len);

  uim_scm_init_proc3("getaddrinfo", c_getaddrinfo);
  uim_scm_init_proc1("freeaddrinfo", c_freeaddrinfo);
  uim_scm_init_proc3("socket", c_socket);
  uim_scm_init_proc3("connect", c_connect);
  uim_scm_init_proc3("bind", c_bind);
  uim_scm_init_proc2("listen", c_listen);
  uim_scm_init_proc2("shutdown", c_shutdown);
  uim_scm_init_proc0("shutdown-how-alist?", c_shutdown_how_alist);
  uim_lisp_shutdown_how_alist = make_arg_list(shutdown_how);
  uim_scm_gc_protect(&uim_lisp_shutdown_how_alist);

  uim_scm_init_proc0("make-sockaddr-storage", c_make_sockaddr_storage);
  uim_scm_init_proc1("delete-sockaddr-storage", c_delete_sockaddr_storage);
  uim_scm_init_proc2("accept", c_accept);

  uim_scm_init_proc1("getpeereid", c_getpeereid);
}

void
uim_plugin_instance_quit(void)
{
  uim_scm_gc_unprotect(&uim_lisp_ai_flags);
  uim_scm_gc_unprotect(&uim_lisp_ai_family);
  uim_scm_gc_unprotect(&uim_lisp_ai_socktype);
  uim_scm_gc_unprotect(&uim_lisp_ai_protocol);
  uim_scm_gc_unprotect(&uim_lisp_shutdown_how_alist);
}

