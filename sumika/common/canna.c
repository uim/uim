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

#include <stdlib.h>
#include <dlfcn.h>
#include <pwd.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <glib.h>
#include "canna.h"

static struct _canna_api RkFunc;
static void   *canna_lib;
static int     context_num;
static void    canna_get_privatedicname(unsigned char **);

int canna_init(char *cannahost) {
    if(canna_lib != NULL)
	dlclose(canna_lib);

    canna_lib = dlopen("libcanna.so.1", RTLD_LAZY);

    if(canna_lib == NULL)
	return -1;

    /* 全部使うとは思えないので必要なものだけ対応づける */
    RkFunc.Initialize     = dlsym(canna_lib, "RkInitialize");
    RkFunc.Finalize       = dlsym(canna_lib, "RkFinalize");
    RkFunc.GetWordTextDic = dlsym(canna_lib, "RkGetWordTextDic");
    RkFunc.ListDic        = dlsym(canna_lib, "RkListDic");
    RkFunc.GetMountList   = dlsym(canna_lib, "RkGetMountList");
    RkFunc.MountDic       = dlsym(canna_lib, "RkMountDic");
    RkFunc.UnmountDic     = dlsym(canna_lib, "RkUnmountDic");
    RkFunc.DefineDic      = dlsym(canna_lib, "RkDefineDic");
    RkFunc.DeleteDic      = dlsym(canna_lib, "RkDeleteDic");
    RkFunc.SyncDic        = dlsym(canna_lib, "RkSync");
    
    if(RkFunc.Initialize     == NULL || RkFunc.Finalize   == NULL ||
       RkFunc.MountDic       == NULL || RkFunc.UnmountDic == NULL ||
       RkFunc.DefineDic      == NULL || RkFunc.DeleteDic  == NULL ||
       RkFunc.GetWordTextDic == NULL || RkFunc.ListDic    == NULL ||
       RkFunc.GetMountList   == NULL || RkFunc.SyncDic    == NULL)
    {
	dlclose(canna_lib);
	return -1;
    }

    context_num = RkFunc.Initialize(cannahost);

    if(context_num >= 0)
	return 0;
    else
	return -1;
}

void canna_close(void) {
    if(RkFunc.Finalize != NULL)
	RkFunc.Finalize();

    if(canna_lib != NULL)
	dlclose(canna_lib);
}

int canna_get_word_text_dic(unsigned char *dirname,
			    unsigned char *dicname,
			    word **head)
{
    unsigned char buf[1024];
    unsigned char dicname_bk[256];
    int ret = 0;

    if(RkFunc.GetWordTextDic == NULL)
	return -1;

#ifndef HAVE_STRLCPY
    strncpy(dicname_bk, dicname, 256);
    dicname_bk[255] = '\0';
#else
    strlcpy(dicname_bk, dicname, 256);
#endif
    do {
	ret = RkFunc.GetWordTextDic(context_num, dirname,
				    dicname_bk, buf, 1024);
	if(ret <= 0)
	    break;
	
	dicname_bk[0] = '\0';
	cannadic_parse_line(buf, head);
    } while(ret >= 0);

    if(ret < 0)
	return -1;
    else
	return 0;
}

word *canna_get_word_text_priv_dic(void) {
    struct passwd *pw;
    char *username, *dirname;
    char dir[] = ":user/";
    int len;
    word *list = NULL;

    pw = getpwuid(getuid());
    if(pw == NULL)
	return NULL;

    username = strdup(pw->pw_name);
    if(username == NULL)
	return NULL;

    /* dirname := ":user/username" */
    len = strlen(dir) + strlen(username) + 1;

    dirname = malloc(sizeof(char) * len);
    if(dirname == NULL)
	return NULL;

    snprintf(dirname, len, "%s%s", dir, username);
    canna_get_word_text_dic(dirname, "user", &list);

    free(dirname);
    free(username);

    return list;
}


int canna_define_dic(unsigned char *dicname, unsigned char *buf) {
    /* buf := phon cclass desc */
    int ret;

    if(RkFunc.MountDic(context_num, (char *)dicname, 0) < 0 )
	return -1;

    ret = RkFunc.DefineDic(context_num, (char *)dicname, buf);

    RkFunc.UnmountDic(context_num, (char *)dicname);

    return ret;
}

int canna_delete_dic(unsigned char *dicname, unsigned char *buf) {
    /* buf := phon cclass desc */
    int ret;

    if(RkFunc.MountDic(context_num, (char *)dicname, 0) < 0 )
	return -1;

    ret = RkFunc.DeleteDic(context_num, (char *)dicname, buf);

    RkFunc.UnmountDic(context_num, (char *)dicname);

    return ret;
}

GList *canna_read_privatediclist(void) {
    struct passwd *pw;
    char *username, *dirname;
    char dir[] = ":user/";
    char dicname[] = "user"; /* XXX */
    GList *list = NULL;
    unsigned char buf[1024];
    unsigned char dicname_bk[256];
    int ret = 0;
    int len;

    if(RkFunc.GetWordTextDic == NULL)
	return NULL;

    pw = getpwuid(getuid());
    if(pw == NULL)
	return NULL;

    username = strdup(pw->pw_name);
    if(username == NULL)
	return NULL;

    /* dirname := ":user/username" */
    len = strlen(dir) + strlen(username) + 1;

    dirname = malloc(sizeof(char) * len);
    if(dirname == NULL)
	return NULL;

    snprintf(dirname, len, "%s%s", dir, username);
#ifndef HAVE_STRLCPY
    strncpy(dicname_bk, dicname, 256);
    dicname_bk[255] = '\0';
#else
    strlcpy(dicname_bk, dicname, 256);
#endif
    do {
	ret = RkFunc.GetWordTextDic(context_num, dirname,
				    dicname_bk, buf, 1024);
	if(ret <= 0)
	    break;
	
	dicname_bk[0] = '\0';
	list = cannadic_parse_line_glist(buf, list);
    } while(ret >= 0);

    free(dirname);
    free(username);

    return list;
}
