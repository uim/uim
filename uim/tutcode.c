/*

  Copyright (c) 2026 uim Project https://github.com/uim/uim

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

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uim.h"
#include "dynlib.h"
#include "gettext.h"
#include "uim-notify.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-util.h"

enum tutcode_bushu_index_kind {
  TUTCODE_BUSHU_EXPAND,
  TUTCODE_BUSHU_INDEX2
};

struct tutcode_bushu_index_entry {
  char *key;
  char *value;
  size_t line_number;
};

struct tutcode_bushu_index {
  char *filename;
  char *encoding;
  enum tutcode_bushu_index_kind kind;
  struct tutcode_bushu_index_entry *entries;
  size_t nr_entries;
  size_t capacity;
  struct tutcode_bushu_index *next;
};

struct tutcode_utf8_char {
  const char *str;
  size_t len;
};

static struct tutcode_bushu_index *tutcode_bushu_indexes;

static void
notify_dictionary_error(const char *filename, const char *message)
{
  uim_notify_info(N_("uim-tutcode: bushu dictionary %s: %s"),
                  filename, message);
}

static void
notify_invalid_line(const char *filename, size_t line_number)
{
  uim_notify_info(N_("uim-tutcode: bushu dictionary %s: invalid line %lu"),
                  filename, (unsigned long)line_number);
}

static char *
read_dictionary_line(FILE *fp, int *read_error)
{
  size_t capacity = 256;
  size_t len = 0;
  char *line = uim_malloc(capacity);
  int ch;

  *read_error = 0;
  while ((ch = fgetc(fp)) != EOF && ch != '\n') {
    if (len + 1 >= capacity) {
      size_t new_capacity;

      if (capacity > ((size_t)-1) / 2) {
        free(line);
        *read_error = 1;
        return NULL;
      }
      new_capacity = capacity * 2;
      line = uim_realloc(line, new_capacity);
      capacity = new_capacity;
    }
    line[len++] = (char)ch;
  }

  if (ch == EOF) {
    if (ferror(fp)) {
      free(line);
      *read_error = 1;
      return NULL;
    }
    if (len == 0) {
      free(line);
      return NULL;
    }
  }

  if (len > 0 && line[len - 1] == '\r')
    len--;
  line[len] = '\0';
  return line;
}

static size_t
utf8_char_length(const unsigned char *str, size_t remaining)
{
  size_t len, i;

  if (remaining == 0)
    return 0;
  if (str[0] < 0x80)
    return 1;
  if (str[0] >= 0xc2 && str[0] <= 0xdf)
    len = 2;
  else if (str[0] >= 0xe0 && str[0] <= 0xef)
    len = 3;
  else if (str[0] >= 0xf0 && str[0] <= 0xf4)
    len = 4;
  else
    return 0;

  if (remaining < len)
    return 0;
  for (i = 1; i < len; i++) {
    if ((str[i] & 0xc0) != 0x80)
      return 0;
  }
  if ((str[0] == 0xe0 && str[1] < 0xa0) ||
      (str[0] == 0xed && str[1] >= 0xa0) ||
      (str[0] == 0xf0 && str[1] < 0x90) ||
      (str[0] == 0xf4 && str[1] >= 0x90))
    return 0;
  return len;
}

static int
compare_utf8_chars(const void *a, const void *b)
{
  const struct tutcode_utf8_char *char_a = a;
  const struct tutcode_utf8_char *char_b = b;
  size_t common_len = char_a->len < char_b->len ? char_a->len : char_b->len;
  /* Lexicographic UTF-8 byte order preserves Unicode code point order. */
  int result = memcmp(char_a->str, char_b->str, common_len);

  if (result)
    return result;
  if (char_a->len < char_b->len)
    return -1;
  if (char_a->len > char_b->len)
    return 1;
  return 0;
}

static char *
normalize_index2_key(const char *key)
{
  size_t key_len = strlen(key);
  size_t nr_chars = 0, capacity = 0, offset = 0, i, output_offset = 0;
  struct tutcode_utf8_char *chars = NULL;
  char *normalized;

  while (offset < key_len) {
    size_t char_len = utf8_char_length((const unsigned char *)key + offset,
                                       key_len - offset);

    if (!char_len) {
      free(chars);
      return NULL;
    }
    if (nr_chars == capacity) {
      size_t new_capacity = capacity ? capacity * 2 : 2;

      if (new_capacity < capacity ||
          new_capacity > ((size_t)-1) / sizeof(*chars)) {
        free(chars);
        return NULL;
      }
      chars = uim_realloc(chars, new_capacity * sizeof(*chars));
      capacity = new_capacity;
    }
    chars[nr_chars].str = key + offset;
    chars[nr_chars].len = char_len;
    nr_chars++;
    offset += char_len;
  }

  if (nr_chars > 1)
    qsort(chars, nr_chars, sizeof(*chars), compare_utf8_chars);

  normalized = uim_malloc(key_len + 1);
  for (i = 0; i < nr_chars; i++) {
    memcpy(normalized + output_offset, chars[i].str, chars[i].len);
    output_offset += chars[i].len;
  }
  normalized[output_offset] = '\0';
  free(chars);
  return normalized;
}

static char *
duplicate_range(const char *str, size_t len)
{
  char *copy = uim_malloc(len + 1);

  memcpy(copy, str, len);
  copy[len] = '\0';
  return copy;
}

static int
parse_dictionary_line(enum tutcode_bushu_index_kind kind,
                      const char *line, char **key, char **value)
{
  size_t line_len = strlen(line);

  *key = NULL;
  *value = NULL;
  if (line_len == 0)
    return 0;

  if (kind == TUTCODE_BUSHU_EXPAND) {
    size_t key_len = utf8_char_length((const unsigned char *)line, line_len);

    if (!key_len)
      return -1;
    *key = duplicate_range(line, key_len);
    *value = uim_strdup(line + key_len);
    return 1;
  }

  if (kind == TUTCODE_BUSHU_INDEX2) {
    const char *separator = strchr(line, ' ');
    char *normalized;

    if (!separator || separator == line)
      return -1;
    *key = duplicate_range(line, (size_t)(separator - line));

    normalized = normalize_index2_key(*key);
    free(*key);
    *key = normalized;
    if (!*key)
      return -1;
    *value = uim_strdup(separator + 1);
    return 1;
  }

  return -1;
}

static int
append_index_entry(struct tutcode_bushu_index *index, char *key, char *value,
                   size_t line_number)
{
  if (index->nr_entries == index->capacity) {
    size_t new_capacity = index->capacity ? index->capacity * 2 : 128;

    if (new_capacity < index->capacity ||
        new_capacity > ((size_t)-1) / sizeof(*index->entries))
      return 0;
    index->entries = uim_realloc(index->entries,
                                 new_capacity * sizeof(*index->entries));
    index->capacity = new_capacity;
  }

  index->entries[index->nr_entries].key = key;
  index->entries[index->nr_entries].value = value;
  index->entries[index->nr_entries].line_number = line_number;
  index->nr_entries++;
  return 1;
}

static int
compare_index_entries(const void *a, const void *b)
{
  const struct tutcode_bushu_index_entry *entry_a = a;
  const struct tutcode_bushu_index_entry *entry_b = b;
  int result = strcmp(entry_a->key, entry_b->key);

  if (result)
    return result;
  if (entry_a->line_number < entry_b->line_number)
    return -1;
  if (entry_a->line_number > entry_b->line_number)
    return 1;
  return 0;
}

static int
finalize_index(struct tutcode_bushu_index *index)
{
  struct tutcode_bushu_index_entry *sorted = index->entries;
  struct tutcode_bushu_index_entry *unique;
  size_t unique_count = 0, start, i;

  if (index->nr_entries < 2)
    return 1;

  qsort(sorted, index->nr_entries, sizeof(*sorted), compare_index_entries);
  unique = uim_malloc(index->nr_entries * sizeof(*unique));

  for (start = 0; start < index->nr_entries;) {
    size_t end = start + 1;

    while (end < index->nr_entries &&
           strcmp(sorted[start].key, sorted[end].key) == 0)
      end++;

    if (index->kind == TUTCODE_BUSHU_EXPAND) {
      size_t selected = start;

      unique[unique_count++] = sorted[selected];
      sorted[selected].key = NULL;
      sorted[selected].value = NULL;
      for (i = start; i < end; i++) {
        free(sorted[i].key);
        free(sorted[i].value);
        sorted[i].key = NULL;
        sorted[i].value = NULL;
      }
    } else {
      char *merged;
      size_t merged_len = 0;

      if (end == start + 1) {
        merged = sorted[start].value;
        sorted[start].value = NULL;
      } else {
        for (i = start; i < end; i++) {
          size_t value_len = strlen(sorted[i].value);

          if (value_len > ((size_t)-1) - merged_len - 1)
            goto error;
          merged_len += value_len;
        }
        merged = uim_malloc(merged_len + 1);
        merged_len = 0;
        for (i = start; i < end; i++) {
          size_t value_len = strlen(sorted[i].value);

          memcpy(merged + merged_len, sorted[i].value, value_len);
          merged_len += value_len;
        }
        merged[merged_len] = '\0';
      }
      unique[unique_count].key = sorted[start].key;
      unique[unique_count].value = merged;
      unique[unique_count].line_number = sorted[start].line_number;
      sorted[start].key = NULL;
      unique_count++;
      for (i = start; i < end; i++) {
        free(sorted[i].key);
        free(sorted[i].value);
        sorted[i].key = NULL;
        sorted[i].value = NULL;
      }
    }
    start = end;
  }

  free(sorted);
  index->entries = unique;
  index->nr_entries = unique_count;
  index->capacity = unique_count;
  return 1;

error:
  for (i = 0; i < unique_count; i++) {
    free(unique[i].key);
    free(unique[i].value);
  }
  free(unique);
  for (i = 0; i < index->nr_entries; i++) {
    free(sorted[i].key);
    free(sorted[i].value);
    sorted[i].key = NULL;
    sorted[i].value = NULL;
  }
  free(sorted);
  index->entries = NULL;
  index->nr_entries = 0;
  index->capacity = 0;
  return 0;
}

static void
free_index(struct tutcode_bushu_index *index)
{
  size_t i;

  if (!index)
    return;
  for (i = 0; i < index->nr_entries; i++) {
    free(index->entries[i].key);
    free(index->entries[i].value);
  }
  free(index->entries);
  free(index->filename);
  free(index->encoding);
  free(index);
}

static struct tutcode_bushu_index *
build_index(const char *filename, const char *encoding,
            enum tutcode_bushu_index_kind kind)
{
  struct tutcode_bushu_index *index;
  FILE *fp;
  void *converter;
  size_t line_number = 0;
  int read_error;
  char *line;

  fp = fopen(filename, "rb");
  if (!fp) {
    char message[256];

    snprintf(message, sizeof(message), _("cannot open file: %s"),
             strerror(errno));
    notify_dictionary_error(filename, message);
    return NULL;
  }

  converter = uim_iconv->create("UTF-8", encoding);
  if (!converter) {
    notify_dictionary_error(filename, _("cannot create character converter"));
    fclose(fp);
    return NULL;
  }

  index = uim_malloc(sizeof(*index));
  memset(index, 0, sizeof(*index));
  index->filename = uim_strdup(filename);
  index->encoding = uim_strdup(encoding);
  index->kind = kind;

  while ((line = read_dictionary_line(fp, &read_error)) != NULL) {
    char *utf8_line, *key, *value;
    int line_nonempty = line[0] != '\0';
    int parse_status;

    line_number++;
    utf8_line = uim_iconv->convert(converter, line);
    free(line);
    if (!utf8_line || (line_nonempty && utf8_line[0] == '\0')) {
      free(utf8_line);
      notify_invalid_line(filename, line_number);
      goto error;
    }

    parse_status = parse_dictionary_line(kind, utf8_line, &key, &value);
    free(utf8_line);
    if (parse_status < 0) {
      notify_invalid_line(filename, line_number);
      goto error;
    }
    if (parse_status == 0)
      continue;
    if (!append_index_entry(index, key, value, line_number)) {
      free(key);
      free(value);
      notify_dictionary_error(filename, _("index is too large"));
      goto error;
    }
  }

  if (read_error) {
    notify_dictionary_error(filename, _("failed while reading file"));
    goto error;
  }
  if (fclose(fp) != 0) {
    fp = NULL;
    notify_dictionary_error(filename, _("failed while closing file"));
    goto error;
  }
  fp = NULL;

  uim_iconv->release(converter);
  converter = NULL;
  if (!finalize_index(index)) {
    notify_dictionary_error(filename, _("failed to finalize index"));
    goto error;
  }
  return index;

error:
  if (fp)
    fclose(fp);
  if (converter)
    uim_iconv->release(converter);
  free_index(index);
  return NULL;
}

static struct tutcode_bushu_index *
get_index(const char *filename, const char *encoding,
          enum tutcode_bushu_index_kind kind)
{
  struct tutcode_bushu_index *index;

  for (index = tutcode_bushu_indexes; index; index = index->next) {
    if (index->kind == kind &&
        strcmp(index->filename, filename) == 0 &&
        strcmp(index->encoding, encoding) == 0)
      return index;
  }

  index = build_index(filename, encoding, kind);
  if (index) {
    index->next = tutcode_bushu_indexes;
    tutcode_bushu_indexes = index;
  }
  return index;
}

static const struct tutcode_bushu_index_entry *
find_index_entry(const struct tutcode_bushu_index *index, const char *key)
{
  size_t min = 0, max = index->nr_entries;

  while (min < max) {
    size_t mid = min + (max - min) / 2;
    int result = strcmp(key, index->entries[mid].key);

    if (result == 0)
      return &index->entries[mid];
    if (result < 0)
      max = mid;
    else
      min = mid + 1;
  }
  return NULL;
}

static uim_lisp
tutcode_bushu_indexed_search(uim_lisp filename_, uim_lisp encoding_,
                             uim_lisp kind_, uim_lisp key_)
{
  const char *filename = REFER_C_STR(filename_);
  const char *encoding = REFER_C_STR(encoding_);
  const char *kind_name = REFER_C_STR(kind_);
  const char *key = REFER_C_STR(key_);
  enum tutcode_bushu_index_kind kind;
  struct tutcode_bushu_index *index;
  const struct tutcode_bushu_index_entry *entry;
  char *normalized_key = NULL;

  if (strcmp(kind_name, "expand") == 0)
    kind = TUTCODE_BUSHU_EXPAND;
  else if (strcmp(kind_name, "index2") == 0)
    kind = TUTCODE_BUSHU_INDEX2;
  else {
    uim_notify_info(N_("uim-tutcode: unknown bushu dictionary kind: %s"),
                    kind_name);
    return uim_scm_f();
  }

  index = get_index(filename, encoding, kind);
  if (!index)
    return uim_scm_f();

  if (kind == TUTCODE_BUSHU_INDEX2) {
    normalized_key = normalize_index2_key(key);
    if (!normalized_key) {
      uim_notify_info(N_("uim-tutcode: invalid UTF-8 bushu lookup key"));
      return uim_scm_f();
    }
    key = normalized_key;
  }

  entry = find_index_entry(index, key);
  free(normalized_key);
  return entry ? MAKE_STR(entry->value) : uim_scm_f();
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc4("tutcode-bushu-lib-indexed-search",
                     tutcode_bushu_indexed_search);
}

void
uim_plugin_instance_quit(void)
{
  while (tutcode_bushu_indexes) {
    struct tutcode_bushu_index *index = tutcode_bushu_indexes;

    tutcode_bushu_indexes = index->next;
    free_index(index);
  }
}
