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

#ifndef __ANTHY_IMPORTWORDPANE_H__
#define __ANTHY_IMPORTWORDPANE_H__

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

typedef struct _anthy_imortwordpane {
	GtkWidget *pane;

	/* Import */
	GtkWidget *importfilename;
	GtkWidget *button_import_getfilename;
	GtkWidget *button_doimport;
/*	GtkWidget *opt_dic_import_type; */
	GtkWidget *combo_box_dic_import_type;

	/* Export */
	GtkWidget *exportfilename;
	GtkWidget *button_export_getfilename;
	GtkWidget *button_doexport;
/*	GtkWidget *opt_dic_export_type; */
	GtkWidget *combo_box_dic_export_type;
} AnthyImportWordPane;

int create_anthy_importwordpane(AnthyImportWordPane *pane);
int show_anthy_importwordpane(AnthyImportWordPane *pane);

#endif /* __ANTHY_IMPORTWORDPANE_H__ */
