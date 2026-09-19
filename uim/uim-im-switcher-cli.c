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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/select.h>

#include "uim.h"
#include "uim-helper.h"

static void
usage(FILE *stream)
{
  fprintf(stream,
          "Usage: uim-im-switcher-cli [-s|--scope SCOPE] INPUT_METHOD\n"
          "       uim-im-switcher-cli --help\n"
          "\n"
          "SCOPE is one of:\n"
          "  desktop      change all running uim contexts (default)\n"
          "  application  change contexts in the focused application\n"
          "  text-area    change the focused text area\n");
}

enum switch_scope {
  SWITCH_SCOPE_DESKTOP,
  SWITCH_SCOPE_APPLICATION,
  SWITCH_SCOPE_TEXT_AREA
};

static int
parse_scope(const char *name, enum switch_scope *scope)
{
  if (strcmp(name, "desktop") == 0) {
    *scope = SWITCH_SCOPE_DESKTOP;
  } else if (strcmp(name, "application") == 0) {
    *scope = SWITCH_SCOPE_APPLICATION;
  } else if (strcmp(name, "text-area") == 0) {
    *scope = SWITCH_SCOPE_TEXT_AREA;
  } else {
    return -1;
  }
  return 0;
}

static const char *
scope_message(enum switch_scope scope)
{
  switch (scope) {
  case SWITCH_SCOPE_DESKTOP:
    return "im_change_whole_desktop\n";
  case SWITCH_SCOPE_APPLICATION:
    return "im_change_this_application_only\n";
  case SWITCH_SCOPE_TEXT_AREA:
    return "im_change_this_text_area_only\n";
  }
  return NULL;
}

static void
print_im_list(const char *message)
{
  const char *line = message;
  int line_number = 0;
  int nr_im = 0;

  printf("Available input methods:\n");

  while (line && *line) {
    const char *end = strchr(line, '\n');
    size_t length = end ? (size_t)(end - line) : strlen(line);
    char *entry;
    char *field;
    char *tab;
    int field_number;
    int selected = 0;

    if (line_number >= 2 && length > 0) {
      entry = uim_malloc(length + 1);
      memcpy(entry, line, length);
      entry[length] = '\0';

      field = entry;
      for (field_number = 0; field_number < 3; field_number++) {
        tab = strchr(field, '\t');
        if (!tab)
          break;
        field = tab + 1;
      }
      if (field_number == 3 && strcmp(field, "selected") == 0)
        selected = 1;

      tab = strchr(entry, '\t');
      if (tab)
        *tab = '\0';
      printf("  %s %s\n", selected ? "*" : " ", entry);
      nr_im++;
      free(entry);
    }

    if (!end)
      break;
    line = end + 1;
    line_number++;
  }

  if (nr_im == 0)
    printf("  (none)\n");
}

static char *
get_im_list(int fd)
{
  uim_helper_send_message(fd, "im_list_get\n");

  for (;;) {
    fd_set readfds;
    struct timeval timeout;
    int result;
    char *message;

    FD_ZERO(&readfds);
    FD_SET(fd, &readfds);
    timeout.tv_sec = 1;
    timeout.tv_usec = 0;

    result = select(fd + 1, &readfds, NULL, NULL, &timeout);
    if (result < 0) {
      if (errno == EINTR)
        continue;
      return NULL;
    }
    if (result == 0)
      return NULL;

    uim_helper_read_proc(fd);
    while ((message = uim_helper_get_message()) != NULL) {
      if (strncmp(message, "im_list\n", 8) == 0) {
        return message;
      }
      free(message);
    }
  }
}

static int
im_list_contains(const char *message, const char *im_name)
{
  const char *line = message;
  int line_number = 0;

  while (line && *line) {
    const char *end = strchr(line, '\n');
    size_t length = end ? (size_t)(end - line) : strlen(line);
    const char *tab;
    size_t name_length;

    if (line_number >= 2 && length > 0) {
      tab = memchr(line, '\t', length);
      name_length = tab ? (size_t)(tab - line) : length;
      if (strlen(im_name) == name_length && strncmp(line, im_name, name_length) == 0)
        return 1;
    }

    if (!end)
      break;
    line = end + 1;
    line_number++;
  }

  return 0;
}

int
main(int argc, char **argv)
{
  const char *im_name;
  const char *scope_name;
  const char *message_header;
  char *message;
  char *im_list;
  enum switch_scope scope = SWITCH_SCOPE_DESKTOP;
  int argument_index = 1;
  int fd;

  if (argc == 1 ||
      (argc == 2 && (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0))) {
    usage(stdout);
    putchar('\n');

    fd = uim_helper_init_client_fd(NULL);
    if (fd < 0) {
      fprintf(stderr, "uim-im-switcher-cli: cannot connect to uim-helper-server\n");
      return EXIT_FAILURE;
    }

    im_list = get_im_list(fd);
    if (!im_list) {
      fprintf(stderr, "uim-im-switcher-cli: timed out waiting for input method list\n");
      uim_helper_close_client_fd(fd);
      return EXIT_FAILURE;
    }
    print_im_list(im_list);
    free(im_list);
    uim_helper_close_client_fd(fd);
    return EXIT_SUCCESS;
  }

  if (argument_index < argc &&
      (strcmp(argv[argument_index], "--scope") == 0 ||
       strcmp(argv[argument_index], "-s") == 0)) {
    if (argument_index + 1 >= argc) {
      usage(stderr);
      return EXIT_FAILURE;
    }
    scope_name = argv[argument_index + 1];
    argument_index += 2;
  } else if (argument_index < argc &&
             strncmp(argv[argument_index], "--scope=", 8) == 0) {
    scope_name = argv[argument_index] + 8;
    argument_index++;
  } else {
    scope_name = NULL;
  }

  if (scope_name && parse_scope(scope_name, &scope) < 0) {
    fprintf(stderr, "uim-im-switcher-cli: invalid scope: %s\n", scope_name);
    return EXIT_FAILURE;
  }

  if (argument_index != argc - 1) {
    usage(stderr);
    return EXIT_FAILURE;
  }

  im_name = argv[argument_index];
  if (im_name[0] == '\0' || strpbrk(im_name, "\t\r\n") != NULL) {
    fprintf(stderr, "uim-im-switcher-cli: invalid input method name\n");
    return EXIT_FAILURE;
  }

  fd = uim_helper_init_client_fd(NULL);
  if (fd < 0) {
    fprintf(stderr, "uim-im-switcher-cli: cannot connect to uim-helper-server\n");
    return EXIT_FAILURE;
  }

  im_list = get_im_list(fd);
  if (!im_list) {
    fprintf(stderr, "uim-im-switcher-cli: timed out waiting for input method list\n");
    uim_helper_close_client_fd(fd);
    return EXIT_FAILURE;
  }
  if (!im_list_contains(im_list, im_name)) {
    print_im_list(im_list);
    fprintf(stderr, "uim-im-switcher-cli: unknown input method: %s\n", im_name);
    free(im_list);
    uim_helper_close_client_fd(fd);
    return EXIT_FAILURE;
  }
  free(im_list);

  message_header = scope_message(scope);
  uim_asprintf(&message, "%s%s\n", message_header, im_name);
  uim_helper_send_message(fd, message);
  free(message);
  uim_helper_close_client_fd(fd);

  return EXIT_SUCCESS;
}
