/*
 *  $Id:$
 *  Copyright (c) 2003,2004 Masahito Omote <omote@utyuuzin.net>
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the name of authors nor the names of its contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 */

#ifndef __TOPWIN_H__
#define __TOPWIN_H__

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "anthycontainer.h"
#include "skkcontainer.h"
#include "addwordpane.h"
#include "uimcontainer.h"

typedef struct topwindow {
    GtkWidget *window;
    GtkWidget *menubar;
    AnthyContainer *anthycontainer;
    SKKContainer *skkcontainer;
    UimConfigContainer *uimconfigcontainer;
    GtkWidget *addwordpane;
    GtkWidget *bt_end;
} TopWindow;

typedef enum {
    MAIN_MENU_FILE_QUIT
} MenuFileItem;

typedef enum {
    MAIN_MENU_OPTION_ANTHY,
    MAIN_MENU_OPTION_SKK,
    MAIN_MENU_OPTION_PRIME,
    MAIN_MENU_OPTION_INTEGRATE,
    MAIN_MENU_OPTION_UIM,
} MenuOptionItem;

typedef enum {
    MAIN_MENU_HELP_ABOUT,
} MenuHelpItem;

/* extern int cb_destroy_topwindow(GtkWidget *widget, void *data); */
extern int create_topwindow(TopWindow *win);
extern int show_topwindow(TopWindow *win);

#endif /* __TOPWIN_H__ */
