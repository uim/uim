/*
  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or
  without modification, are permitted provided that the
  following conditions are met:

  1. Redistributions of source code must retain the above
     copyright notice, this list of conditions and the
     following disclaimer.
  2. Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the
     following disclaimer in the documentation and/or other
     materials provided with the distribution.
  3. Neither the name of authors nor the names of its
     contributors may be used to endorse or promote products
     derived from this software without specific prior written
     permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <locale.h>

#include <uim/uim.h>
#include <uim/uim-util.h>
#include <uim/uim-im-switcher.h>

#include "uim-el-agent.h"

/* called when owner buffer is killed  */
static int
cmd_release(int context_id)
{
  return release_uim_agent_context(context_id);
}


/* process helper message received by uim-el-helper-agent */
static int
cmd_helper(int context_id, char *helper_message)
{
  uim_agent_context *ua;
  int ret;

  ua = get_uim_agent_context(context_id);
  ret = helper_handler(ua, helper_message);

  if (ua && ret) {
    show_commit_string_uim_agent_context(current);
    show_preedit_uim_agent_context(ua);
    show_candidate_uim_agent_context(ua);

    check_prop_list_update(ua);
    check_default_engine();

    return ret;
  }

  return 0;
}


/*
 * remove Emacs local focus from current context
 */
static int
cmd_unfocused(int context_id)
{
  uim_agent_context *ua = get_uim_agent_context(context_id);

  if (ua) {
	/* keep preedit if exists */
	show_preedit_uim_agent_context(ua);
	show_candidate_uim_agent_context(ua);
	check_default_engine();
	return clear_current_uim_agent_context();
  } else {
	/* error */
	return -1;
  }
}


/*
 * set Emacs local focus to the context
 */
static int
cmd_focused(int context_id)
{
  uim_agent_context *ua = get_uim_agent_context(context_id);

  if (set_current_uim_agent_context(ua) > 0) {
	show_preedit_uim_agent_context(ua);
	show_candidate_uim_agent_context(ua);

	check_prop_list_update(ua);
	check_default_engine();
	return 1;
  } else {
	return -1;
  }
}

static int
cmd_hide(int context_id)
{
  uim_agent_context *ua = get_uim_agent_context(context_id);

  if (ua != NULL)
	return 1;
  else
	return -1;
}


static int
cmd_show(int context_id)
{
  uim_agent_context *ua = get_uim_agent_context(context_id);

  if (ua != NULL) {
	show_preedit_uim_agent_context(ua);
	show_candidate_uim_agent_context(ua);
	return 1;
  } else {
	return -1;
  }
}


static int
cmd_new(int context_id, const char *encoding)
{

  uim_agent_context *ua;

  if (get_uim_agent_context(context_id)) {
	debug_printf(DEBUG_WARNING, "context %d already exists\n", context_id);
	return -1;
  }

  ua = new_uim_agent_context(context_id, encoding);

  if (ua) {
	check_prop_list_update(ua);
	check_default_engine();
	return 1;
  } else {
	return -1;
  }

}


static int
cmd_reset(int context_id)
{
  /* reset current context */
  uim_agent_context *ua = get_uim_agent_context(context_id);

  if (ua != NULL) {
	/* before reset, clear preedit and candidate */
	clear_preedit(ua->pe);
	clear_candidate(ua->cand);

	uim_reset_context(ua->context);

	check_prop_list_update(ua);
	check_default_engine();
	return 1;
  } else {
	return -1;
  }
}


static int
cmd_change(int context_id, const char *im)
{
  uim_agent_context *ua;

  if (im && strlen(im) > 0 
	  && (ua = get_uim_agent_context(context_id))) {

	if (check_im_name(im)) {
	  switch_context_im(ua, im);

	  check_prop_list_update(ua);
	  check_default_engine();

	  return 1;
	} else {
	  return -1;	 
 	}
  } else {
	return -1;
  }
}


static int
cmd_list(void)
{
  output_default_im_engine();

  if (list_im_engine() > 0)
	return 1;
  else
	return -1;
}


static int
cmd_setenc(char *opt)
{
  char *im, *encoding;

  im = opt;
  if (im == NULL)
	return -1;

  if ((encoding = strchr(opt, ' ')) == NULL || *(encoding + 1) == '\0') {
	/* 2nd arg not found */
	return -1;
  } else {
	*encoding = '\0';
	encoding ++;
  }

  if (set_im_encoding(im, encoding) > 0)
	return 1;
  else
	return -1;
}

static int
cmd_getenc(const char *im)
{
  const char *encoding = get_im_encoding(im);

  if (encoding)
	a_printf(" ( E \"%s\" ) ", encoding);
  else
	a_printf(" ( E \"\" ) ");

  return 1;
}


static int
cmd_prop(int context_id, const char *prop)
{
  uim_agent_context *ua;

  if (prop && strlen(prop) > 0 &&
	  (ua = get_uim_agent_context(context_id))) {
	uim_prop_activate(ua->context, prop);

	show_im_uim_agent_context(ua);
	show_prop_uim_agent_context(ua);
	show_preedit_uim_agent_context(ua);
	show_candidate_uim_agent_context(ua);

	return 1;
  } else {
	return -1;
  }
}


static int
cmd_label(int context_id)
{
  uim_agent_context *ua;

  if ((ua = get_uim_agent_context(context_id))) {
	show_im_uim_agent_context(ua);
	show_prop_uim_agent_context(ua);
	show_preedit_uim_agent_context(ua);
	show_candidate_uim_agent_context(ua);
	return 1;
  } else {
	return -1;
  }
}


static int
cmd_nop(int context_id)
{
  uim_agent_context *ua = get_uim_agent_context(context_id);

  if (ua) {
	show_im_uim_agent_context(ua);
	show_prop_uim_agent_context(ua);
	show_preedit_uim_agent_context(ua);
	show_candidate_uim_agent_context(ua);
	return 1;
  } else {
	return -1;
  }  

  return 1;
}

static int
cmd_error(void)
{
  return -1;
}


static void
check_default_engine(void)
{
  if (default_engine_updated) {
	output_default_im_engine();
	default_engine_updated = 0;
  }
}


static void
check_prop_list_update(uim_agent_context *ua)
{
  debug_printf(DEBUG_NOTE, "check_prop_list_update\n");

  if (ua && ua->prop->list_update) {
	show_prop_uim_agent_context(ua);

	announce_prop_list_update(ua->prop, ua->encoding);
	ua->prop->list_update = 0;

	show_im_uim_agent_context(ua);
  }
}


static int
process_command(int serial, int cid, char *cmd)
{
  /*char *opt, *opt2; */
  char *p, *opt;
  int ret;

  if ((p = strchr(cmd, '\n')))
	*p = '\0';
  else
	return -1;

  if ((opt = strchr(cmd, ' '))) {
	*opt = '\0';
	opt ++;
  } else {
	opt = NULL;
  }

  debug_printf(DEBUG_NOTE, "cmd: %s\n", cmd);

  if (strcmp(cmd, "RELEASE") == 0)
	ret = cmd_release(cid);
  else if (strcmp(cmd, "UNFOCUSED") == 0)
	ret = cmd_unfocused(cid);
  else if (strcmp(cmd, "FOCUSED") == 0)
	ret = cmd_focused(cid);
  else if (strcmp(cmd, "HIDE") == 0)
	ret = cmd_hide(cid);
  else if (strcmp(cmd, "SHOW") == 0)
	ret = cmd_show(cid);
  else if (strcmp(cmd, "NEW") == 0)
  	ret = cmd_new(cid, opt);
  else if (strcmp(cmd, "RESET") == 0)
	ret = cmd_reset(cid);
  else if (strcmp(cmd, "CHANGE") == 0)
  	ret = cmd_change(cid, opt);
  else if (strcmp(cmd, "PROP") == 0)
  	ret = cmd_prop(cid, opt);
  else if (strcmp(cmd, "LABEL") == 0)
  	ret = cmd_label(cid);
  else if (strcmp(cmd, "HELPER") == 0)
	ret = cmd_helper(cid, opt);
  else if (strcmp(cmd, "NOP") == 0)
  	ret = cmd_nop(cid);
  else if (strcmp(cmd, "LIST") == 0)
  	ret = cmd_list();
  else if (strcmp(cmd, "SETENC") == 0)
  	ret = cmd_setenc(opt);
  else if (strcmp(cmd, "GETENC") == 0)
  	ret = cmd_getenc(opt); /* for debug */
  else
	ret = cmd_error();

  return ret;
}



static int
analyze_keyvector(char *vector, uim_key *ukey, char *keyname, size_t keyname_len)
{
  char *p1, *p2, *c;
  int key;

  p1 = vector + 1;

  if (strstr(p1, "mouse-")) {
	debug_printf(DEBUG_NOTE, "mouse event\n");
	return 1;
  }

  /* check space */
  if ((p2 = strchr(vector, ' '))) {
	/* if there is a space, 1st string must be 27 */
	key = strtol(p1, &c, 10);
	if (c != p2 || key != 27) {
	  debug_printf(DEBUG_WARNING, "1st key is not 27\n");
	  return -1;
	} else {
	  ukey->mod |= UMod_Alt;
	}
	p1 = p2 + 1; 
  }

  if (*p1 == ']') {
	debug_printf(DEBUG_WARNING, "invalid key vector\n");
	return -1;
  }

  key = strtol(p1, &c, 10);

  if (*c == ']') {
	/* all of rest characters are digit */
	if (key & (1<<27)) ukey->mod |= UMod_Alt;
	/* if(key & (1<<27)) mod |= UMod_Meta; */
	if (key & (1<<26)) ukey->mod |= UMod_Control;
	if (key & (1<<25)) ukey->mod |= UMod_Shift;
	if (key & (1<<24)) ukey->mod |= UMod_Hyper;
	if (key & (1<<23)) ukey->mod |= UMod_Super;
	if (key & (1<<22)) ukey->mod |= UMod_Alt;

	ukey->key = key & 0xff;

	if (ukey->key == 27) {
	  ukey->key = UKey_Escape;
	} else if (ukey->key <= 26) {
	  ukey->mod |= UMod_Control;
	  if (ukey->key == 0)
		ukey->key = 32;
	  else
		ukey->key += 96;
	} else if (ukey->key >= 0x41 && ukey->key <= 0x5a) {
	  ukey->mod |= UMod_Shift;
	}
  } else {
	/* contains non digit characters */
	while (1) {
	  if (*(p1 + 1) == '-') {
		switch (*p1) {
		case 'S':
		  ukey->mod |= UMod_Shift;
		  break;
		case 'C':
		  ukey->mod |= UMod_Control;
		  break;
		case 'A':
		  ukey->mod |= UMod_Alt;
		  break;
		case 'M':
		  /* mod |= UMod_Meta; */
		  ukey->mod |= UMod_Alt;
		  break;
		case 's':
		  ukey->mod |= UMod_Super;
		  break;
		case 'H':
		  ukey->mod |= UMod_Hyper;
		  break;
		case 'X':
		  /* dummy; */
		  break;
		default:
		  return -1;
		  break;
		}
		p1 += 2;
	  } else {
		if ((p2 = strrchr(p1, ']')) == NULL || p1 == p2) {
		  /* [C-] [C-M-] */
		  debug_printf(DEBUG_WARNING, "invalid key vector\n");
		  return -1;
		} else {
		  *p2 = '\0';

		  if (strlen(p1) == 1) {
			/* single character [C-a] [C-1] */
			ukey->key = *p1;
		  } else {
			/* [C-next] [C-M-end] */
			if (*p1 >= 'A' && *p1 <= 'Z')
			  /* if the first character is upper case, 
				 it can be considered as XEmacs style abbrev */
				convert_keyname_a2e(keyname, p1, keyname_len);
			else
			strlcpy(keyname, p1, keyname_len);

			ukey->key = convert_keyname_e2u(keyname);
		  }

		  break;
		}
	  }
	}
  }

  if (ukey->key < 0) return -1;
  
  return 1;
}


static int
process_keyvector(int serial, int cid, uim_key ukey, const char *keyname)
{
  int ret, ret2;

  if (! focused ||
	  current == NULL || 
	  (current != NULL && current->context_id != cid)) {

	if (set_current_uim_agent_context(get_uim_agent_context(cid)) < 0) {
	  debug_printf(DEBUG_WARNING, "context %d not found\n", cid);
	  return -1;
	}
  }

  focused = 1;


  if (ukey.key >= 0) {
	/* key input is received by requested context */
	debug_printf(DEBUG_NOTE, "uim_press_key\n");
	ret = uim_press_key(current->context, ukey.key, ukey.mod);

	debug_printf(DEBUG_NOTE, "uim_release_key\n");
	ret2 = uim_release_key(current->context, ukey.key, ukey.mod);

	debug_printf(DEBUG_NOTE, "ret = %d, ret2 = %d\n", ret, ret2);

	show_commit_string_uim_agent_context(current);

	if (ret > 0) {
	  /* uim did not process the key */

	  if (ukey.mod & UMod_Shift && ukey.key >= 0x41 && ukey.key <= 0x5a)
		ukey.mod &= ~UMod_Shift;

	  if (ukey.mod != 0 || ukey.key > 255) {

		a_printf(" ( n [(");

		if (ukey.mod & UMod_Control) a_printf("control ");
		if (ukey.mod & UMod_Alt) a_printf("meta ");
		/* if (ukey->mod & UMod_Shift) a_printf("shift "); */
		if (ukey.mod & UMod_Hyper) a_printf("hyper ");
		if (ukey.mod & UMod_Super) a_printf("super ");

		if (ukey.key > 255)
		  a_printf("%s", keyname);
		else
		  a_printf("%d", ukey.key);

		a_printf(")] ) ");

	  } else {
		a_printf(" ( n [%d] ) ", ukey.key);
	  }
			
	}
	show_preedit_uim_agent_context(current);
	show_candidate_uim_agent_context(current);
  } else {
	/* ukey.key < 0 */
	show_commit_string_uim_agent_context(current);
	show_preedit_uim_agent_context(current);
	show_candidate_uim_agent_context(current);
	a_printf(" ( n ) "); /* dummy */
  }

  /*show_prop_uim_agent_context(current);*/
  check_prop_list_update(current);

  check_default_engine();

  return 1;
}



void
cleanup(void)
{
  uim_quit();
}


int
main(int argc, char *argv[])
{
  int opt;

  setlocale(LC_CTYPE, "");

  while ((opt = getopt(argc, argv, "d")) != -1) {
	switch (opt) {
	case 'd':
	  debug_level ++;
	  break;
	}
  }

  if (debug_level == 0) fclose(stderr);

  if (uim_init() < 0) {
	debug_printf(DEBUG_ERROR, "uim_init failed\n");
	return -1;
  }

  atexit(cleanup);


  a_printf("OK\n");

  while (1) {
	int cid, serial;
	char *p1, *p2, *c;
	char buf[2048], keyname[32];
	uim_key ukey;

	fflush(stdout);

	if (fgets(buf, sizeof (buf), stdin) == NULL) {
	  if (feof(stdin))
	    debug_printf(DEBUG_NOTE, "unexpected EOF\n");
	  else
	    debug_printf(DEBUG_ERROR, "failed to read command: %s\n",
			 strerror (errno));
	  goto QUIT;
	}

	p1 = buf;
	serial = -1;

	/*
	  command format 
	    serial CID COMMAND OPTION

	  key format
	    serial CID [keyvector]
	*/

	if ((p2 = strchr(p1, ' ')) == NULL) {
	  debug_printf(DEBUG_WARNING, "input error: space after 1st string\n");
	  goto ERROR;
	}

	/* 1st string must be digit */
	*p2 = '\0';
	serial = strtol(p1, &c, 10);
	if (c != p2) {
	  debug_printf(DEBUG_WARNING, "input error: invalid serial %d\n", serial);
	  goto ERROR;
	}

	p1 = p2 + 1;
	if ((p2 = strchr(p1, ' ')) == NULL) {
	  debug_printf(DEBUG_WARNING, "input error: no space after 2nd string\n");
	  goto ERROR;
	}

	/* 2nd string must be digit */
	*p2 = '\0';
	cid = strtol(p1, &c, 10);
	if (c != p2) {
	  debug_printf(DEBUG_WARNING, "invalid cid %d\n", cid);
	  goto ERROR;
	}

	/* 3rd string */
	p1 = p2 + 1;

	if (*p1 == '[') {
	  /* keyvector if 3rd string starts with [  */

	  if ((p2 = strchr(p1, ']')) == NULL) {
		/* no corresponding ]  */
		debug_printf(DEBUG_WARNING, "']' not found\n");
		goto ERROR;
	  }

	  p2 ++; 
	  if (*p2 == ']') p2 ++; /* for [X-]] */
	  *p2 = '\0';   /* replace character after ] with \0  */

	  ukey.mod = 0;
	  ukey.key = -1;
	  keyname[0] = '\0';


	  if (analyze_keyvector(p1, &ukey, keyname, sizeof(keyname)) > 0) {

	  	a_printf("( %d %d ", serial, cid);
		if (process_keyvector(serial, cid, ukey, keyname) < 0)
		  a_printf(" ( f ) ");
		else
		  a_printf(" ( a ) ");

		a_printf(" )\n");
		fflush(stdout);

		continue;
	  }

	  goto ERROR;


	} else if (*p1 >= 'A' && *p1 <= 'Z') {
	  /* command */

	  if (strncmp(p1, "QUIT", 4) == 0) goto QUIT;

	  a_printf("( %d %d ", serial, cid);
	  if (process_command(serial, cid, p1) < 0) {
		debug_printf(DEBUG_WARNING, "command error\n");
		a_printf(" ( f ) ");   /* command error */
	  } else {
		a_printf(" ( a ) ");   /* command ok */
	  }

	  a_printf(" )\n");
	  fflush(stdout);

	  continue;
	}
  
	debug_printf(DEBUG_WARNING, "invalid input\n");
	
  ERROR:
	a_printf("( %d 0 ( x ) )\n", serial);
	fflush(stdout);
  }

 QUIT:

  uim_quit();
  return 0;
}

