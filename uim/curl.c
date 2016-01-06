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

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>

#include <curl/curl.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "gettext.h"
#include "dynlib.h"


#ifdef DEBUG
#define DPRINTFN(n,x)  if ((n) <= verbose_level()) fprintf x;
#else
#define DPRINTFN(n,x)
#endif

static uim_lisp uim_curl_fetch_simple(uim_lisp);
static void *uim_curl_fetch_simple_internal(void *);

struct uim_curl_post_args {
  uim_lisp url;
  uim_lisp post;
};
static uim_lisp uim_curl_post(uim_lisp, uim_lisp);
static void *uim_curl_post_internal(struct uim_curl_post_args *);

static uim_lisp uim_curl_url_escape(uim_lisp);
static void *uim_curl_url_escape_internal(void *);

static uim_lisp uim_curl_url_unescape(uim_lisp);
static void *uim_curl_url_unescape_internal(void *);

void uim_plugin_instance_init(void);
void uim_plugin_instance_quit(void);

struct curl_memory_struct {
  char *str;
  size_t size;
};
static size_t uim_curl_write_func(void *, size_t, size_t, void *);
static CURLcode uim_curl_perform(CURL *);

static size_t
uim_curl_write_func(void *ptr, size_t size, size_t nmemb, void *data)
{
  struct curl_memory_struct *mem = (struct curl_memory_struct *)data;
  size_t realsize = size * nmemb;

  /*
   * We know that it isn't possible to overflow during multiplication if
   * neither operand uses any of the most significant half of the bits in
   * a size_t.
   */
  if((unsigned long long)((nmemb | size) &
	((unsigned long long)SIZE_MAX << (sizeof(size_t) << 2))) &&
     (realsize / size != nmemb))
    return 0;

  if(SIZE_MAX - mem->size - 1 < realsize)
    realsize = SIZE_MAX - mem->size - 1;

  if(mem->str != NULL)
    mem->str = uim_realloc(mem->str, mem->size + realsize + 1);
  else
    mem->str = uim_malloc(realsize + 1);

  if(mem->str != NULL) {
    memcpy(&(mem->str[mem->size]), ptr, realsize);
    mem->size += realsize;
    mem->str[mem->size] = '\0';
  }

  return realsize;
}

static uim_lisp
uim_curl_fetch_simple(uim_lisp url_)
{
  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_curl_fetch_simple_internal,
						    (void *)url_);
}

static void *
uim_curl_fetch_simple_internal(void *url_)
{
  const char *url = REFER_C_STR((uim_lisp)url_);
  CURL *curl;
  CURLcode res;
  struct curl_memory_struct chunk;
  uim_lisp fetched_str_;

  curl = curl_easy_init();

  if(curl == NULL)
    return uim_scm_f();

  memset(&chunk, 0, sizeof(struct curl_memory_struct));

  curl_easy_setopt(curl, CURLOPT_URL, url);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, uim_curl_write_func);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);

  res = uim_curl_perform(curl);

  fetched_str_ = (chunk.str != NULL) ? MAKE_STR(chunk.str) : uim_scm_f();

  curl_easy_cleanup(curl);
  curl_global_cleanup();
  free(chunk.str);

  return (void *)fetched_str_;
}

static CURLcode
uim_curl_perform(CURL *curl)
{
  uim_bool use_proxy;
  char *ua;
  char *referer;
  char *http_proxy;
  CURLcode res;

  ua = uim_scm_symbol_value_str("uim-curl-user-agent");
  referer = uim_scm_symbol_value_str("uim-curl-referer");
  use_proxy = uim_scm_symbol_value_bool("uim-curl-use-proxy?");
  http_proxy = uim_scm_symbol_value_str("uim-curl-http-proxy");

  curl_easy_setopt(curl, CURLOPT_USERAGENT,
		   (ua != NULL) ? ua : "libcurl-agent/1.0");
  curl_easy_setopt(curl, CURLOPT_REFERER,
		   (referer != NULL) ? referer : "");
  if (use_proxy) {
    curl_easy_setopt(curl, CURLOPT_PROXY,
		     (http_proxy != NULL) ? http_proxy : "");
  }

  res = curl_easy_perform(curl);

  free(ua);
  free(referer);
  free(http_proxy);

  return res;
}

static uim_lisp
uim_curl_post(uim_lisp url_, uim_lisp post_)
{
  struct uim_curl_post_args args;

  args.url = url_;
  args.post = post_;
  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_curl_post_internal,
						    &args);
}

static void *
uim_curl_post_internal(struct uim_curl_post_args *args)
{
  uim_lisp post_ = args->post;
  uim_lisp post_car_, post_cdr_;
  uim_lisp fetched_str_;
  const char *url = REFER_C_STR(args->url);
  CURL *curl;
  CURLcode res;
  struct curl_memory_struct chunk;
  struct curl_httppost* post_first = NULL;
  struct curl_httppost* post_last = NULL;

  curl = curl_easy_init();

  if(curl == NULL)
    return uim_scm_f();

  memset(&chunk, 0, sizeof(struct curl_memory_struct));

  curl_easy_setopt(curl, CURLOPT_URL, url);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, uim_curl_write_func);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);

  for(post_cdr_ = post_;
      !NULLP(post_cdr_);
      post_cdr_ = CDR(post_cdr_)) {
      const char *name, *value;
      post_car_ = CAR(post_cdr_);

      name = REFER_C_STR(CAR(post_car_));
      value = REFER_C_STR(CDR(post_car_));

      curl_formadd(&post_first, &post_last,
		   CURLFORM_COPYNAME, name,
		   CURLFORM_COPYCONTENTS, value,
		   CURLFORM_END);
  }

  curl_easy_setopt(curl, CURLOPT_HTTPPOST, post_first);

  res = uim_curl_perform(curl);

  fetched_str_ = (chunk.str != NULL) ? MAKE_STR(chunk.str) : uim_scm_f();

  curl_easy_cleanup(curl);
  curl_formfree(post_first);
  curl_global_cleanup();
  free(chunk.str);

  return (void *)fetched_str_;
}

static uim_lisp
uim_curl_url_escape(uim_lisp url_)
{
  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_curl_url_escape_internal,
						    (void *)url_);
}
static void *
uim_curl_url_escape_internal(void *url_)
{
  uim_lisp escaped_url_;
  const char *unescaped_url = REFER_C_STR((uim_lisp)url_);
  char *escaped_url;
  CURL *curl;

  curl = curl_easy_init();

  if(curl == NULL)
    return uim_scm_f();

  escaped_url = curl_easy_escape(curl, unescaped_url, strlen(unescaped_url));
  escaped_url_ = (escaped_url != NULL) ? MAKE_STR(escaped_url) : uim_scm_f();

  curl_free(escaped_url);
  curl_easy_cleanup(curl);
  curl_global_cleanup();

  return (void *)escaped_url_;
}

static uim_lisp
uim_curl_url_unescape(uim_lisp url_)
{
  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_curl_url_unescape_internal,
						    (void *)url_);
}

static void *
uim_curl_url_unescape_internal(void *url_)
{
  uim_lisp unescaped_url_;
  const char *escaped_url = REFER_C_STR((uim_lisp)url_);
  char *unescaped_url;
  int len; /* curl_easy_unescape uses int, not size_t */
  CURL *curl;

  curl = curl_easy_init();

  if(curl == NULL)
    return uim_scm_f();

  unescaped_url = curl_easy_unescape(curl, escaped_url,
				     strlen(escaped_url), &len);
  unescaped_url_ = (len > 0) ? MAKE_STR(unescaped_url) : uim_scm_f();

  curl_free(unescaped_url);
  curl_easy_cleanup(curl);
  curl_global_cleanup();

  return (void *)unescaped_url_;
}

void uim_plugin_instance_init(void)
{
  uim_scm_init_proc1("curl-fetch-simple", uim_curl_fetch_simple);
  uim_scm_init_proc1("curl-url-escape", uim_curl_url_escape);
  uim_scm_init_proc1("curl-url-unescape", uim_curl_url_unescape);
  uim_scm_init_proc2("curl-post", uim_curl_post);
}

void uim_plugin_instance_quit(void)
{
  return;
}
