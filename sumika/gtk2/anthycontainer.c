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

#include <gtk/gtk.h>

#include "anthycontainer.h"

#include "uim/config.h"
#include "uim/gettext.h"

void create_anthycontainer(AnthyContainer *container) {
    GtkWidget *label_viewword, *label_addword, *label_importword;

    container->container = gtk_notebook_new();
    container->viewwordpane = g_malloc(sizeof(AnthyViewWordPane));
    container->importwordpane = g_malloc(sizeof(AnthyImportWordPane));

    container->addwordpane = anthy_addwordpane_new();
    create_viewwordpane(container->viewwordpane);
    create_anthy_importwordpane(container->importwordpane);

    gtk_notebook_set_tab_pos(GTK_NOTEBOOK(container->container), GTK_POS_TOP);
    gtk_container_set_border_width(GTK_CONTAINER(container->container), 10);
    gtk_container_add(GTK_CONTAINER(container->container), (container->viewwordpane)->pane);

    label_viewword = gtk_label_new(_("View word"));
    gtk_widget_show(label_viewword);
    gtk_notebook_set_tab_label(GTK_NOTEBOOK(container->container),
			       gtk_notebook_get_nth_page(GTK_NOTEBOOK(container->container), 0),
			       label_viewword);

    gtk_container_add(GTK_CONTAINER(container->container), (container->addwordpane)->pane);
    label_addword = gtk_label_new(_("Add word"));
    gtk_widget_show(label_addword);
    gtk_notebook_set_tab_label(GTK_NOTEBOOK(container->container),
			       gtk_notebook_get_nth_page(GTK_NOTEBOOK(container->container), 1),
			       label_addword);

    gtk_container_add(GTK_CONTAINER(container->container), (container->importwordpane)->pane);
    label_importword = gtk_label_new(_("Import/Export"));
    gtk_widget_show(label_importword);
    gtk_notebook_set_tab_label(GTK_NOTEBOOK(container->container),
			       gtk_notebook_get_nth_page(GTK_NOTEBOOK(container->container), 2),
			       label_importword);

}

void show_anthycontainer(AnthyContainer *container, gboolean flags) {
    g_return_if_fail(container);
    g_return_if_fail(container->addwordpane);
    g_return_if_fail(container->viewwordpane);

    if(flags) {
	gtk_widget_show(container->container);
	gtk_widget_show((container->viewwordpane)->pane);
	gtk_widget_show((container->addwordpane)->pane);
	gtk_widget_show((container->importwordpane)->pane);
    }
}

void clean_anthycontainer(AnthyContainer *container) {
    g_return_if_fail(container);
    g_return_if_fail(container->addwordpane);
    g_return_if_fail(container->viewwordpane);

    g_free(container->addwordpane);
    g_free(container->viewwordpane);
}
