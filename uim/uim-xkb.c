/*

  Copyright (c) 2007-2013 uim Project http://code.google.com/p/uim/

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

#include <string.h>

#include <X11/XKBlib.h>

#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "dynlib.h"
#include "uim-x-util.h"

static XkbDescPtr xkb = NULL;

static uim_lisp
xkb_set_display(uim_lisp lisp_display)
{
    Display *c_display = (Display *)C_PTR(lisp_display);

    if (! XkbQueryExtension(c_display, NULL, NULL, NULL, NULL, NULL))
	return uim_scm_f();
    if (xkb != NULL) XkbFreeKeyboard(xkb, XkbAllComponentsMask, True);
    if ((xkb = XkbAllocKeyboard()) == NULL) return uim_scm_f();
    xkb->dpy = c_display;

    return uim_scm_t();
}

static uim_lisp
xkb_levels(XkbDescPtr xkb, KeyCode kc, int group)
{
    int l, nlevels = XkbKeyGroupWidth(xkb, kc, group);
    uim_lisp levels = uim_scm_null();

    for (l = nlevels - 1; l >= 0; l--) {
	int ukey = uim_x_keysym2ukey(XkbKeySymEntry(xkb, kc, l, group));
	levels = CONS(MAKE_INT(ukey), levels);
    }

    return levels;
}

static uim_lisp
xkb_groups(XkbDescPtr xkb, KeyCode kc)
{
    char name[XkbKeyNameLength + 1];
    int g, ngroups;
    uim_lisp groups;

    name[XkbKeyNameLength] = '\0';
    strncpy(name, xkb->names->keys[kc].name, XkbKeyNameLength);
    if (name[0] == '\0') return uim_scm_f();

    ngroups = XkbKeyNumGroups(xkb, kc);
    if (ngroups == 0) return uim_scm_f();
    groups = uim_scm_null();
    for (g = ngroups - 1; g >= 0; g--)
	groups = CONS(xkb_levels(xkb, kc, g), groups);

    return CONS(MAKE_INT(kc), CONS(MAKE_SYM(name), groups));
}

static uim_lisp
xkb_lib_display_readyp(void)
{
    return MAKE_BOOL(xkb != NULL && xkb->dpy != NULL);
}

/*
 * returns a list of which each element is of the form
 *
 *     (xkeycode xkbname group1 group2 ...)
 *
 * in which each of groupn is a list of keysyms at different shift
 * levels available in the group, and xkbname, a scheme symbol, is a
 * symbolic key name as returned by XKB.  Keysyms are returned after
 * being converted to uim keys which can differ from X keysyms.
 */
static uim_lisp
xkb_lib_get_map(void)
{
    int kc;
    uim_lisp map;

    if (xkb == NULL || xkb->dpy == NULL) return uim_scm_f();

    if (XkbGetUpdatedMap(xkb->dpy, XkbAllClientInfoMask, xkb) != Success)
	return uim_scm_f();
    if (XkbGetNames(xkb->dpy, XkbKeyNamesMask, xkb) != Success)
	return uim_scm_f();

    map = uim_scm_null();
    for (kc = xkb->max_key_code; kc >= xkb->min_key_code; kc--) {
	uim_lisp groups = xkb_groups(xkb, kc);
	if (TRUEP(groups)) map = CONS(groups, map);
    }

    return map;
}

static uim_lisp
xkb_open_display(void)
{
    Display *display = XkbOpenDisplay(NULL, NULL, NULL, NULL, NULL, NULL);
    if (display == NULL) return uim_scm_f();
    return MAKE_PTR(display);
}

void
uim_dynlib_instance_init(void)
{
    uim_scm_init_proc0("xkb-lib-display-ready?", xkb_lib_display_readyp);
    uim_scm_init_proc0("xkb-lib-get-map", xkb_lib_get_map);

    uim_scm_init_proc1("%xkb-set-display", xkb_set_display);
    uim_scm_init_proc0("%xkb-open-display", xkb_open_display);
}

void
uim_dynlib_instance_quit(void)
{
    if (xkb != NULL) XkbFreeKeyboard(xkb, XkbAllComponentsMask, True);
    xkb = NULL;
}
