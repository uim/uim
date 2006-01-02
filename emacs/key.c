/*
  Copyright (c) 2005-2006 uim Project http://uim.freedesktop.org/

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

#include "key.h"

/*
  convert abbreviated key description to generic Emacs-style key description
  (for XEmacs)
*/
void
convert_keyname_a2e(char *keyname, const char *name)
{
  if (strcmp(name, "BS") == 0)
	strcpy(keyname, "backspace");
  else if (strcmp(name, "TAB") == 0)
	strcpy(keyname, "tab");
  else if (strcmp(name, "RET") == 0)
	strcpy(keyname, "return");
  else if (strcmp(name, "ESC") == 0)
	strcpy(keyname, "escape");
  else if (strcmp(name, "DEL") == 0)
	strcpy(keyname, "delete");
  else if (strcmp(name, "DEL") == 0)
	strcpy(keyname, "delete");
  else if (strcmp(name, "SPC") == 0)
	strcpy(keyname, "space");
  else
	strcpy(keyname, name);
}


/*
  convert generic Emacs-style key description to Uim-style key description
*/
enum UKey
convert_keyname_e2u(const char *keyname)
{
  if (strcmp("space", keyname) == 0)
	return ' ';  /* "space" has no Uim-style key description */
  else if (strcmp("backspace", keyname) == 0)
	return UKey_Backspace;
  else if (strcmp("tab", keyname) == 0)
	return UKey_Tab;
  else if (strcmp("return", keyname) == 0)
	return UKey_Return;
  else if (strcmp("escape", keyname) == 0)
	return UKey_Escape;
  else if (strcmp("prior", keyname) == 0)
	return UKey_Prior;
  else if (strcmp("next", keyname) == 0)
	return UKey_Next;
  else if (strcmp("end", keyname) == 0)
	return UKey_End;
  else if (strcmp("home", keyname) == 0)
	return UKey_Home;
  else if (strcmp("left", keyname) == 0)
	return UKey_Left;
  else if (strcmp("up", keyname) == 0)
	return UKey_Up;
  else if (strcmp("right", keyname) == 0)
	return UKey_Right;
  else if (strcmp("down", keyname) == 0)
	return UKey_Down;
  else if (strcmp("insert", keyname) == 0)
	return UKey_Insert;
  else if (strcmp("delete", keyname) == 0)
	return UKey_Delete;
  else if (strcmp("muhenkan", keyname) == 0)
	return UKey_Muhenkan;
  else if (strcmp("henkan", keyname) == 0)
	return UKey_Henkan_Mode;
  else if (strcmp("zenkaku-hankaku", keyname) == 0)
	return UKey_Zenkaku_Hankaku;
  else if (strcmp("f1", keyname) == 0)
	return UKey_F1;
  else if (strcmp("f2", keyname) == 0)
	return UKey_F2;
  else if (strcmp("f3", keyname) == 0)
	return UKey_F3;
  else if (strcmp("f4", keyname) == 0)
	return UKey_F4;
  else if (strcmp("f5", keyname) == 0)
	return UKey_F5;
  else if (strcmp("f6", keyname) == 0)
	return UKey_F6;
  else if (strcmp("f7", keyname) == 0)
	return UKey_F7;
  else if (strcmp("f8", keyname) == 0)
	return UKey_F8;
  else if (strcmp("f9", keyname) == 0)
	return UKey_F9;
  else if (strcmp("f10", keyname) == 0)
	return UKey_F10;
  else if (strcmp("f11", keyname) == 0)
	return UKey_F11;
  else if (strcmp("f12", keyname) == 0)
	return UKey_F12;
  else if (strcmp("f13", keyname) == 0)
	return UKey_F13;
  else if (strcmp("f14", keyname) == 0)
	return UKey_F14;
  else if (strcmp("f15", keyname) == 0)
	return UKey_F15;
  else if (strcmp("f16", keyname) == 0)
	return UKey_F16;
  else if (strcmp("f17", keyname) == 0)
	return UKey_F17;
  else if (strcmp("f18", keyname) == 0)
	return UKey_F18;
  else if (strcmp("f19", keyname) == 0)
	return UKey_F19;
  else if (strcmp("f20", keyname) == 0)
	return UKey_F20;
  else if (strcmp("f21", keyname) == 0)
	return UKey_F21;
  else if (strcmp("f22", keyname) == 0)
	return UKey_F22;
  else if (strcmp("f23", keyname) == 0)
	return UKey_F23;
  else if (strcmp("f24", keyname) == 0)
	return UKey_F24;
  else if (strcmp("f25", keyname) == 0)
	return UKey_F25;
  else if (strcmp("f26", keyname) == 0)
	return UKey_F26;
  else if (strcmp("f27", keyname) == 0)
	return UKey_F27;
  else if (strcmp("f28", keyname) == 0)
	return UKey_F28;
  else if (strcmp("f29", keyname) == 0)
	return UKey_F29;
  else if (strcmp("f30", keyname) == 0)
	return UKey_F30;
  else if (strcmp("f31", keyname) == 0)
	return UKey_F31;
  else if (strcmp("f32", keyname) == 0)
	return UKey_F32;
  else if (strcmp("f33", keyname) == 0)
	return UKey_F33;
  else if (strcmp("f34", keyname) == 0)
	return UKey_F34;
  else if (strcmp("f35", keyname) == 0)
	return UKey_F35;
  else
	return UKey_Other;
}
