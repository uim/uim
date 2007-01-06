/*

  Copyright (c) 2003-2007 uim Project http://uim.freedesktop.org/

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

#ifndef _os_dep_h_included_
#define _os_dep_h_included_

#ifdef __cplusplus
extern "C" {
#endif

#ifndef HAVE_GETPEEREID
#include <sys/types.h>
#define getpeereid	uim_getpeereid
int getpeereid(int , uid_t *, gid_t *);
#endif

#ifndef HAVE_SETENV
#define setenv	uim_setenv
int setenv(const char *, const char *, int);
#endif

#ifndef HAVE_UNSETENV
#define unsetenv	uim_unsetenv
void unsetenv(const char *);
#endif

#ifndef HAVE_STRSEP
#define strsep	uim_strsep
char *strsep(char **stringp, const char *delim);
#endif

#ifndef HAVE_STRLCPY
#define strlcpy	uim_strlcpy
size_t strlcpy(char *dst, const char *src, size_t siz);
#endif

#ifndef HAVE_STRLCAT
#define strlcat	uim_strlcat
size_t strlcat(char *dst, const char *src, size_t siz);
#endif

#ifdef __cplusplus
}
#endif
#endif
