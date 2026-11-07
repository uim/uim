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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


/*
 * Drives an input method through libuim for the tests: it reads
 * commands from the standard input and writes what the input method
 * does to the standard output, a line for each.
 *
 * Usage: im-driver IM-NAME SCHEME
 *
 * SCHEME is evaluated before the input method starts, to configure it.
 *
 * Commands:
 *   key NAME       Press and release a key such as "a", "space",
 *                  "return" or "f7". "C-" and "S-" prefixes add Control
 *                  and Shift.
 *   action NAME    Activate an action such as
 *                  "action_google-cgiapi-jp_kana".
 *
 * Output lines, separated by tabs:
 *   preedit[ ATTR TEXT]...
 *   commit TEXT
 *   candidates[ CANDIDATE]...
 *   select INDEX
 *   deactivate
 *   end            The command has been processed.
 *
 * "ready" is written when the input method can take commands.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <uim/uim.h>
#include <uim/uim-scm.h>
#include <uim/uim-util.h>

static uim_context uc;

static void
commit_cb(void *ptr, const char *str)
{
  printf("commit\t%s\n", str);
}

static void
preedit_clear_cb(void *ptr)
{
  printf("preedit");
}

static void
preedit_pushback_cb(void *ptr, int attr, const char *str)
{
  printf("\t%d\t%s", attr, str);
}

static void
preedit_update_cb(void *ptr)
{
  printf("\n");
}

static void
candidate_activate_cb(void *ptr, int nr, int display_limit)
{
  int i;

  printf("candidates");
  for (i = 0; i < nr; i++) {
    uim_candidate cand = uim_get_candidate(uc, i, i);
    printf("\t%s", uim_candidate_get_cand_str(cand));
    uim_candidate_free(cand);
  }
  printf("\n");
}

static void
candidate_select_cb(void *ptr, int index)
{
  printf("select\t%d\n", index);
}

static void
candidate_shift_page_cb(void *ptr, int direction)
{
}

static void
candidate_deactivate_cb(void *ptr)
{
  printf("deactivate\n");
}

static const struct {
  const char *name;
  int key;
} keys[] = {
  {"space", ' '},
  {"left", UKey_Left},
  {"return", UKey_Return},
  {"right", UKey_Right},
  {"f6", UKey_F6},
  {"f7", UKey_F7},
  {"f8", UKey_F8},
  {"f9", UKey_F9},
  {"f10", UKey_F10},
  {NULL, 0},
};

static int
parse_key(const char *name, int *key, int *state)
{
  int i;

  *state = 0;
  while (name[0] != '\0' && name[1] == '-') {
    switch (name[0]) {
    case 'C':
      *state |= UMod_Control;
      break;
    case 'S':
      *state |= UMod_Shift;
      break;
    case 'A':
      *state |= UMod_Alt;
      break;
    default:
      return 0;
    }
    name += 2;
  }
  for (i = 0; keys[i].name; i++) {
    if (strcmp(name, keys[i].name) == 0) {
      *key = keys[i].key;
      return 1;
    }
  }
  if (name[0] != '\0' && name[1] == '\0') {
    *key = (unsigned char)name[0];
    return 1;
  }
  return 0;
}

int
main(int argc, char **argv)
{
  char line[256];

  if (argc != 3) {
    fprintf(stderr, "Usage: %s IM-NAME SCHEME\n", argv[0]);
    return EXIT_FAILURE;
  }

  /* The test reads the output as soon as it's written. */
  setvbuf(stdout, NULL, _IOLBF, 0);

  if (uim_init() < 0) {
    fprintf(stderr, "uim_init() failed\n");
    return EXIT_FAILURE;
  }
  uim_scm_eval_c_string(argv[2]);
  uc = uim_create_context(NULL, "UTF-8", NULL, argv[1], uim_iconv, commit_cb);
  if (!uc) {
    fprintf(stderr, "can't create a context of %s\n", argv[1]);
    uim_quit();
    return EXIT_FAILURE;
  }
  uim_set_preedit_cb(uc,
                     preedit_clear_cb,
                     preedit_pushback_cb,
                     preedit_update_cb);
  uim_set_candidate_selector_cb(uc,
                                candidate_activate_cb,
                                candidate_select_cb,
                                candidate_shift_page_cb,
                                candidate_deactivate_cb);
  printf("ready\n");

  while (fgets(line, sizeof(line), stdin)) {
    char *argument;
    int key, state;

    line[strcspn(line, "\n")] = '\0';
    argument = strchr(line, ' ');
    if (!argument) {
      fprintf(stderr, "invalid command: %s\n", line);
      break;
    }
    *argument++ = '\0';
    if (strcmp(line, "key") == 0 && parse_key(argument, &key, &state)) {
      uim_press_key(uc, key, state);
      uim_release_key(uc, key, state);
    } else if (strcmp(line, "action") == 0) {
      uim_prop_activate(uc, argument);
    } else {
      fprintf(stderr, "invalid command: %s %s\n", line, argument);
      break;
    }
    printf("end\n");
  }

  uim_release_context(uc);
  uim_quit();
  return EXIT_SUCCESS;
}
