/*

  Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "context.h"

/*This file is for find entry from table such Japanese roma-ji table*/

static struct table_info *tables[16];

struct table_info {
  void *addr;
  size_t size;
  char *filename;
};

static int
open_table_internal(const char *fn)
{
  struct table_info *table;
  struct stat st;
  int fd;
  int i;
  void *addr;

  if (lstat(fn, &st) == -1) {
    return 0;
  }

  fd = open(fn, O_RDONLY);
  if (fd == -1) {
    return 0;
  }

  addr = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
  close(fd);
  if (addr == MAP_FAILED) {
    return 0;
  }
  table = (struct table_info *)malloc(sizeof(struct table_info));
  table->addr = addr;
  table->size = st.st_size;
  table->filename = strdup(fn);

  for(i=0;i<16;i++){
    if(!tables[i]) {
      tables[i] = table;
      return i;
    }
  }
  return 0;
}

static void
close_table(struct table_info *table)
{
  munmap(table->addr, table->size);
  free(table->filename);
  free(table);
}

static char *
find_line_head(struct table_info *table, char *from)
{
  while(table->addr <= (void *)from){

    if( *from == '\n') {
      from++;
      return from;
    }
    from--;
  }
  return from;
}

static int
find_table(const char *fn)
{
  int i;
  for(i=0;i<16;i++){
    if(tables[i] && strcmp(tables[i]->filename, fn ) == 0)
      return i;
  }
  return -1;
}

static char *
find_next_linefeed(struct table_info *table, char *from)
{
  while(((char *)(table->addr) + table->size) >= from){

    if( *from == '\n') {
      return from;
    }
    from++;
  }
  return from;
}

static char *
cut_off_line_from_table(struct table_info *table, char *line_head)
{
  char *line_tail;
  char *line;
  size_t line_size;

  line_tail = find_next_linefeed(table, line_head);

  line_size = sizeof(char) * (line_tail - line_head);

  line = (char *)malloc( line_size + 1 );

  memcpy(line, line_head, line_size);

  line[line_size] = '\0';
  return line;
}

/* lenear searching, but it will be enough. */

static LISP
find_matched_lines_from_table_internal(int table_id, char *str, int flag)
{
  struct table_info *table = tables[table_id];
  char *p = table->addr;
  
  while(*(p+ strlen(str))){
    
    if(!strncmp(p, str, strlen(str))){     
      char *tmp = cut_off_line_from_table(table,
					  find_line_head(table, p));
      LISP res = strcons(strlen(tmp), tmp);
      /*      free(tmp); */
      return res;
    }

    if(flag || *(p+ strlen(str)) == '\t') {
      p = find_next_linefeed(table, p);
    }
    p++;
  }
  return NIL;
 
}

/* Followings are API for scheme */

/* this function will return entry which complete matched which first matched.
   So, if there are plural complete matched entry, latter will simply ignored. */

static LISP
find_entry_matched_complete(LISP table_id_, LISP str_)
{
  int table_id = get_c_int(table_id_); 
  char *str = get_c_string(str_);

  return find_matched_lines_from_table_internal(table_id, str, 1);
}

/* this function will return NIL if no entry matched. 
   This function will not return if matched completely. */
static LISP
find_entry_matched_continual(LISP table_id_, LISP str_)
{
  int table_id = get_c_int(table_id_); 
  char *str = get_c_string(str_);
  
  return find_matched_lines_from_table_internal(table_id, str, 0);
}


static LISP
open_table(LISP filename_)
{
  char *filename = get_c_string(filename_);
  int table_id;

  if((table_id = find_table(filename))<0) {
    table_id = open_table_internal(filename);
  }
  return intcons(table_id);
}

void
uim_init_table_subrs()
{
  int i;
  for(i=0;i<16;i++){tables[i] = NULL;}
  init_subr_1("open-table", open_table);
  init_subr_2("find-entry-matched-complete", find_entry_matched_complete);
  init_subr_2("find-entry-matched-continual", find_entry_matched_continual);
}
