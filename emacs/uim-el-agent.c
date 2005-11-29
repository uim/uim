/*
  Copyright (c) 2005 uim Project http://uim.freedesktop.org/

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


#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <locale.h>

#include <sys/select.h>

#include <uim/uim.h>
#include <uim/uim-helper.h>
#include <uim/uim-util.h>
#include <uim/uim-im-switcher.h>

#include "uim-el-agent.h"


static int
cmd_release(int context_id)
{
  if (release_uim_agent_context(context_id) < 0)
	return -1;
  else
	return 1;
}


static int
cmd_unfocused(int context_id)
{
  int ret;
  uim_agent_context *ua;

  /* if context_id is 0, 
	 this function unfocuses from current focused context 
	 otherwise,
	 compare current focused context and unfocuses if they are matching.
  */

  if (current != NULL) {
	ua = current;
	if (context_id != 0 && get_uim_agent_context(context_id) != current)
	  ret = -1;
	else
	  ret = unfocused();
  } else {
	/* already unfocused */
	if ((ua = get_uim_agent_context(context_id)))
	  ret = context_id;
	else
	  ret = -1;
  }
	
  if (ret > 0)
	if (context_id != 0) {
	  show_preedit(ua->pe);
	  return 1;
	} else {
	  return 1;
	}

  else
	return -1;
}


static int
cmd_focused(int context_id)
{
  uim_agent_context *ua = get_uim_agent_context(context_id);

  if (focused(ua) > 0) {
	if (show_preedit(ua->pe))
	  show_candidate(ua->pe->cand);
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
	if (show_preedit(ua->pe))
	  show_candidate(ua->pe->cand);
	return 1;
  } else {
	return -1;
  }
}


static int
cmd_new(int context_id, const char *encoding)
{
  if (get_uim_agent_context(context_id)) {
	debug_printf(DEBUG_WARNING, "context %d already exists\n", context_id);
	return -1;
  } else {
	if (new_uim_agent_context(context_id, encoding) != NULL)
	  return 1;
	else
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
	clear_candidate(ua->pe->cand);
	uim_reset_context(ua->context);
	return 1;
  } else {
	return -1;
  }
}


static int
cmd_change(int context_id, const char *im, const char *encoding)
{
  uim_agent_context *ua;

  if (im && encoding && strlen(im) > 0 
	  && (ua = get_uim_agent_context(context_id))) {

	focused(ua);

	if (check_im_name(im)) {
	  switch_context_im(ua, im, encoding);
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
  if (list_im_engine() > 0)
	return 1;
  else
	return -1;
}


static int
cmd_setenc(const char *im, const char *encoding)
{
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
	return 1;
  } else {
	return -1;
  }
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
check_prop(void)
{
  if (current == NULL) return;

  if (current->prop->list_update) {
	announce_prop_list_update(current->prop, current->encoding);
	output_prop_list(current->prop, current->im);
	current->prop->list_update = 0;
  }

  if (current->prop->label_update) {
	announce_prop_label_update(current->prop, current->encoding);
	current->prop->label_update = 0;
  }
}


static int
process_command(int serial, int cid, char *cmd)
{
  char *opt, *opt2;
  int ret;

  if ((opt = strchr(cmd, ' '))) {
	*opt = '\0';
	opt ++;
	if ((opt2 = strchr(opt, ' '))) {
	  *opt2 = '\0';
	  opt2 ++;
	} else {
	  opt2 = NULL;
	}
  } else {
	opt = NULL;
	opt2 = NULL;
  }

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
  	ret = cmd_change(cid, opt, opt2);
  else if (strcmp(cmd, "LIST") == 0)
  	ret = cmd_list();
  else if (strcmp(cmd, "SETENC") == 0)
  	ret = cmd_setenc(opt, opt2);
  else if (strcmp(cmd, "GETENC") == 0)
  	ret = cmd_getenc(opt); /* for debug */
  else if (strcmp(cmd, "PROP") == 0)
  	ret = cmd_prop(cid, opt);
  else
	ret = cmd_error();

  check_prop();

  check_default_engine();

  return ret;
}


static int
analyze_keyvector(char *vector, uim_key *ukey, char *keyname)
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
			  convert_keyname_a2e(keyname, p1);
			else
			  strcpy(keyname, p1);

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
  int ret;

  if (current == NULL || 
	  (current != NULL && current->context_id != cid)) {

	if (focused(get_uim_agent_context(cid)) < 0) {
	  debug_printf(DEBUG_WARNING, "context %d not found\n", cid);
	  return -1;
	}
  }


  if (ukey.key >= 0) {
	/* key input is received by requested context */
	ret = uim_press_key(current->context, ukey.key, ukey.mod);
	uim_release_key(current->context , ukey.key, ukey.mod);

	if (ret > 0) {
	  /* uim did not process the key */

	  if (current->pe->head == NULL || current->pe->length == 0) {
		/* no preedit */

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

	  } else {
		/* discard the key if preedit exists */
		show_preedit(current->pe);
		show_candidate(current->pe->cand);
		/* a_printf(" ( n ) "); */
	  }
			
	} else {
	  /* key has been processed by uim */
	  show_preedit(current->pe);
	  show_candidate(current->pe->cand);
	}
  } else {
	/* ukey.key < 0 */
	show_preedit(current->pe);
	show_candidate(current->pe->cand);
	a_printf(" ( n ) ");
  }

  check_prop();

  check_default_engine();

  return 1;
}



static void
wait_until_data_arrival(fd_set *rfds)
{
  int fdmax = STDIN_FILENO;

  if (helper_fd > 0) {
	FD_ZERO(rfds);
	FD_SET(helper_fd, rfds);
	if (helper_fd > fdmax) fdmax = helper_fd;
  }

  FD_SET(STDIN_FILENO, rfds);
  
  if (select(fdmax + 1, rfds, NULL, NULL, NULL) < 0) {
	debug_printf(DEBUG_ERROR, "select error\n");
  }
}


static int
take_one_line_from_buffer(char *buf, char *input)
{
  char *p = strchr(buf, '\n'); 
  int rest; 

  if (!p) {
	debug_printf(DEBUG_ERROR, "something wrong occured\n");
	return 0;
  }

  *p = '\0';
  strcpy(input, buf);

  rest = strlen(p + 1);
  memmove(buf, (p + 1), rest);

  return rest;
}

/*
static int
uim_agent_init(void)
{
  init_default_engine();
  return 1;
}
*/


int
main(int argc, char *argv[])
{
  int opt;

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

  /*  uim_agent_init();*/

  a_printf("OK\n");

  while (1) {
	int cid, serial;
	char *p1, *p2, *c;
	int offset = 0, more_input = 0;
	char input[256], buf[256], keyname[32];
	uim_key ukey;
	fd_set rfds;

	fflush(stdout);

	buf[sizeof(buf) - 1] = '\0';

	if (!more_input) {
	  check_helper_connection();   /* keep helper connection */

	  wait_until_data_arrival(&rfds);

	  if (FD_ISSET(STDIN_FILENO, &rfds)) {
		/* read from STDIN if input data exists  */
		read(STDIN_FILENO, buf + offset, sizeof(buf) - offset - 1);
	  } else if (FD_ISSET(helper_fd, &rfds)) {
		/* process helper message only when no input data */
		output_enable = 0;
		helper_handler();
		output_enable = 1;
	  }
	}

	if ((p1 = strchr(buf, '\n'))) {
	  offset = take_one_line_from_buffer(buf, input);
	  more_input = strchr(buf, '\n') ? 1 : 0;
	} else {
	  /* wait \n */
	  offset = strlen(buf);
	  more_input = 0;
	  continue;
	}

	p1 = input;
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


	  if (analyze_keyvector(p1, &ukey, keyname) > 0) {

	  	a_printf("( %d %d ", serial, cid);
		if (process_keyvector(serial, cid, ukey, keyname) < 0)
		  a_printf(" ( f ) ");
		else
		  a_printf(" ( a ) ");

		a_printf(" )\n");

		continue;
	  }

	  goto ERROR;


	} else if (*p1 >= 'A' && *p1 <= 'Z') {
	  /* command */

	  a_printf("( %d %d ", serial, cid);
	  if (process_command(serial, cid, p1) < 0) {
		debug_printf(DEBUG_WARNING, "command error\n");
		a_printf(" ( f ) ");   /* command error */
	  } else {
		a_printf(" ( a ) ");   /* command ok */
	  }

  a_printf(" )\n");

	  continue;
	}
  
	debug_printf(DEBUG_WARNING, "invalid input\n");
	
  ERROR:
	a_printf("( %d 0 ( x ) )\n", serial);
  }


  uim_quit();
  return 0;
}

