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
#include <fcntl.h>
#include <pwd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "skk.h"

struct _skkdic_info {
    int fd;
    char *data;
    size_t len;
    int first;
    int boarder;
/*    struct skk_line head; */
} *skk_dic;

static char *get_skkdicfullpath(void) {
    char *homedirectory, *skkdicfullpath;
    char skkdicname[] = ".skk-jisyo";

    homedirectory = getenv("HOME");
    if(!homedirectory) {
	struct passwd *pw;
	pw = getpwuid(getuid());
	homedirectory = strdup(pw->pw_dir);
	free(pw);
    }

    skkdicfullpath = malloc(strlen(homedirectory) + strlen(skkdicname) + 2);
    if(skkdicfullpath == NULL)
	return NULL;

    snprintf(skkdicfullpath,
	     strlen(homedirectory) + strlen(skkdicname) + 2,
	     "%s/%s", homedirectory, skkdicname);

    return skkdicfullpath;
}

int read_skk_dic(word **head) {
    FILE *fp;
    int fd;
    struct stat st;
    char *ptr;
    void *addr;
    int len, buflen;
    char *skkdicfullpath;
    char buf[256], desc[256], phon[256];
    int okuri = 0;

    skkdicfullpath = get_skkdicfullpath();
    if(skkdicfullpath == NULL) {
	*head = NULL;
	return -1;
    }

    if(lstat(skkdicfullpath, &st) == -1) {
	free(skkdicfullpath);
	return -1;
    }

    fp = fopen(skkdicfullpath, "r");

    free(skkdicfullpath);

    if(!fp) {
	*head = NULL;
	return -1;
    }

    *head = NULL;
    while(fgets(buf, sizeof(buf), fp)) {
	if(buf[0] != '#' && buf[0] != ';') {
	    sscanf(buf, "%s %s", phon, desc); /* XXX */

	    word_append(head, WORD_TYPE_SKK,
			phon, desc, NULL, 0, okuri, NULL); /* XXX */
	} else {
	    if(strstr(buf, "okuri-ari")) {
		okuri = 0;
	    }
	    if(strstr(buf, "okuri-nasi")) {
		okuri = 1;
	    }
	}
    }
    fclose(fp);
    return 0;

}

int write_skk_dic(word *head) {
    char *skkdicfullpath;
    word *pos;
    FILE *fp;
    int fd;
    struct flock lock;

    skkdicfullpath = get_skkdicfullpath();
    if(skkdicfullpath == NULL) {
	return -1;
    }

    fp = fopen(skkdicfullpath, "w");
    free(skkdicfullpath);
    if(!fp) {
	return -1;
    }

    fd = fileno(fp);
    lock.l_type = F_WRLCK;
    lock.l_start = 0;
    lock.l_whence = SEEK_SET;
    lock.l_len = 0;

    if(fcntl(fd, F_SETLK, &lock) < 0) {
	fprintf(stderr, "Lock failed: %s\n", strerror(errno));
	fclose(fp);
	return -1;
    }

    fprintf(fp, ";; okuri-ari entries.\n");
    for(pos = head; pos != NULL; pos = pos->next) {
	if(pos->okuri == 0)
	    fprintf(fp, "%s %s\n", pos->phon, pos->desc);
    }
    fprintf(fp, ";; okuri-nasi entries.\n");
    for(pos = head; pos != NULL; pos = pos->next) {
	if(pos->okuri == 1)
	    fprintf(fp, "%s %s\n", pos->phon, pos->desc);
    }

    lock.l_type = F_UNLCK;
    if(fcntl(fd, F_SETLK, &lock) < 0) {
	fprintf(stderr, "Unlock failed: %s\n", strerror(errno));
	fclose(fp);
	return -1;
    }

    fclose(fp);
    return 0;
}

/*
int read_line(char *ptr, ptrlen, char *buf, int len) {
    int i = 0;
    for(i = 0; i < len; i++, ptr++) {
	if(i >= ptrlen) break;
	if(*ptr == '\0' || *ptr == '\n') {
	    buf[i] = '\0';
	    i++;
	    break;
	}
	buf[i] = *ptr;
    }
    return i;
}
*/
