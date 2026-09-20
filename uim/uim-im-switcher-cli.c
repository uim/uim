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

#include "uim.h"
#include "uim-im-switcher.h"
#include "uim-helper.h"

static void
usage(FILE *stream)
{
  fprintf(stream,
          "Usage: uim-im-switcher-cli [-s|--scope SCOPE] INPUT_METHOD\n"
          "       uim-im-switcher-cli [-h|--help]\n"
          "       uim-im-switcher-cli [-l|--list]\n"
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
print_im_list(uim_context uc)
{
  const char *current_im_name;
  char *current_im_name_copy;
  int nr_im;
  int i;
  int nr_printed = 0;

  printf("Available input methods:\n");

  current_im_name = uim_get_current_im_name(uc);
  current_im_name_copy = current_im_name ? uim_strdup(current_im_name) : NULL;
  nr_im = uim_get_nr_im(uc);
  for (i = 0; i < nr_im; i++) {
    const char *name = uim_get_im_name(uc, i);

    if (!name)
      continue;
    printf("  %s %s\n",
           current_im_name_copy && strcmp(name, current_im_name_copy) == 0 ? "*" : " ",
           name);
    nr_printed++;
  }

  free(current_im_name_copy);

  if (nr_printed == 0)
    printf("  (none)\n");
}

static int
im_list_contains(uim_context uc, const char *im_name)
{
  int nr_im = uim_get_nr_im(uc);
  int i;

  for (i = 0; i < nr_im; i++) {
    const char *name = uim_get_im_name(uc, i);

    if (name && strcmp(name, im_name) == 0)
      return 1;
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
  uim_context uc;
  enum switch_scope scope = SWITCH_SCOPE_DESKTOP;
  int argument_index = 1;
  int fd;

  if (uim_init() < 0) {
    fprintf(stderr, "uim-im-switcher-cli: failed to initialize libuim\n");
    return EXIT_FAILURE;
  }

  uc = uim_create_context(NULL, "UTF-8", NULL, NULL, NULL, NULL);
  if (!uc) {
    fprintf(stderr, "uim-im-switcher-cli: failed to create uim context\n");
    uim_quit();
    return EXIT_FAILURE;
  }

  if (argc == 1 ||
      (argc == 2 && (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0))) {
    usage(stdout);
    uim_release_context(uc);
    uim_quit();
    return EXIT_SUCCESS;
  }

  if (argc == 2 &&
      (strcmp(argv[1], "--list") == 0 || strcmp(argv[1], "-l") == 0)) {
    print_im_list(uc);
    uim_release_context(uc);
    uim_quit();
    return EXIT_SUCCESS;
  }

  if (argument_index < argc &&
      (strcmp(argv[argument_index], "--scope") == 0 ||
       strcmp(argv[argument_index], "-s") == 0)) {
    if (argument_index + 1 >= argc) {
      usage(stderr);
      uim_release_context(uc);
      uim_quit();
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
    uim_release_context(uc);
    uim_quit();
    return EXIT_FAILURE;
  }

  if (argument_index != argc - 1) {
    usage(stderr);
    uim_release_context(uc);
    uim_quit();
    return EXIT_FAILURE;
  }

  im_name = argv[argument_index];
  if (im_name[0] == '\0' || strpbrk(im_name, "\t\r\n") != NULL) {
    fprintf(stderr, "uim-im-switcher-cli: invalid input method name\n");
    uim_release_context(uc);
    uim_quit();
    return EXIT_FAILURE;
  }

  if (!im_list_contains(uc, im_name)) {
    print_im_list(uc);
    fprintf(stderr, "uim-im-switcher-cli: unknown input method: %s\n", im_name);
    uim_release_context(uc);
    uim_quit();
    return EXIT_FAILURE;
  }

  fd = uim_helper_init_client_fd(NULL);
  if (fd < 0) {
    fprintf(stderr, "uim-im-switcher-cli: cannot connect to uim-helper-server\n");
    uim_release_context(uc);
    uim_quit();
    return EXIT_FAILURE;
  }

  message_header = scope_message(scope);
  uim_asprintf(&message, "%s%s\n", message_header, im_name);
  uim_helper_send_message(fd, message);
  free(message);
  uim_helper_close_client_fd(fd);
  uim_release_context(uc);
  uim_quit();

  return EXIT_SUCCESS;
}
