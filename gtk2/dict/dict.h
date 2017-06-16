/*

  Copyright (c) 2004-2013 uim Project https://github.com/uim/uim

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

#ifndef UIM_DICT_DICT_H
#define UIM_DICT_DICT_H

#include "word.h"

typedef struct _uim_dict       uim_dict;
typedef struct _uim_dict_class uim_dict_class;

struct _uim_dict {
  uim_dict_class *funcs;

  char           *identifier;
  char           *filename;

  char           *charset;

  unsigned int    ref_count;

  uim_word       *word_list;
};

struct _uim_dict_class {
  const char * const type;

  /* init or exit backend library */
  void         (*init)              (void);
  void         (*exit)              (void);

  /*
   * list available dictionaries
   * Return: identifier strings terminated by NULL.
   */
  const char **(*list_known_dict)   (void);
  const char  *(*default_priv_dict) (void);

  /* for known dictionaries */
  uim_dict    *(*open)              (const char *identifier);

  /* load a specified file */
  int         *(*is_supported)      (const char *filename);
  uim_dict    *(*load_file)         (const char *filename);
  uim_dict    *(*save_file)         (const char *filename);

  /* destructor */
  void         (*close)             (uim_dict *dict);

  /* for words */
  int          (*add_word)          (uim_dict *dict, uim_word *word);
  int          (*change_word)       (uim_dict *dict, uim_word *word);
  int          (*remove_word)       (uim_dict *dict, uim_word *word);
  void         (*refresh)           (uim_dict *refresh);
};

/* constructor and destructor */
uim_dict *uim_dict_open        (const char *identifier);
void      uim_dict_close       (uim_dict *dict);

uim_dict *uim_dict_ref         (uim_dict *dict);
void      uim_dict_unref       (uim_dict *dict);

/* for words */
int       uim_dict_add_word    (uim_dict *dict, uim_word *word);
int       uim_dict_change_word (uim_dict *dict, uim_word *word);
int       uim_dict_remove_word (uim_dict *dict, uim_word *word);
void      uim_dict_refresh     (uim_dict *dict);

typedef enum {
  DICT_ENUM_DICTIONARY_TYPE_ANTHY,
  DICT_ENUM_DICTIONARY_TYPE_CANNA,
  DICT_ENUM_DICTIONARY_TYPE_SKK,
  DICT_ENUM_DICTIONARY_TYPE_PRIME,
  DICT_ENUM_DICTIONARY_TYPE_UNKOWN
} DictEnumDictionaryType;

#endif /* UIM_DICT_DICT_H */
