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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "uimcontainer.h"
#include "uimconfig.h"

#include "uim/config.h"
#include "uim/uim.h"
#include "uim/uim-scm.h"
#include "uim/gettext.h"

#define SYMBOL_EQ(custom_sym, str) \
	  uim_scm_eq(custom_sym, uim_scm_intern_c_str(str))
#define Q_SYMBOL_EQ(custom_sym, str) \
	  uim_scm_eq(custom_sym, uim_scm_qintern_c_str(str))

static GtkWidget* uimconfig_uim_custom_widget_new (const char *);
static GtkWidget* uimconfig_uim_custom_widget_new_by_sym(const uim_lisp);

static void uimconfig_create_custom_filechooser_widget (GtkWidget *,
							int, uim_lisp);
static void uimconfig_create_custom_spinbutton_widget  (GtkWidget *,
							int, uim_lisp);
static void uimconfig_create_custom_textbox_widget     (GtkWidget *,
							int, uim_lisp);
static void uimconfig_create_custom_combobox_widget    (GtkWidget *,
							int, uim_lisp);

extern void uimapi_gc_protect(uim_lisp *location);

GList *uim_custom_widgetlist_append(GList *, enum UCustomType,
				    GtkWidget *, uim_lisp);

typedef struct _custom_group_list {
    enum UCustomType type;
    GtkWidget *widget;
    char *group_str;
    char *symbol_str;
} Uim_CustomWidgetList;

GList *custom_widget_list = NULL;

static struct _uim_config uim_config;

/* gc_protect'ed variables */
static uim_lisp groups, group_sym;
static uim_lisp cur, item;
static uim_lisp label, default_val, desc, range;
static uim_lisp group, customs, custom_sym, rcustoms;

/* callbacks */
static void cb_uim_specify_check_button_clicked	   (GtkWidget *, gpointer);
static void cb_uim_config_pane_save_button_clicked (GtkButton *, UimConfigContainer *);
static void cb_uim_custom_filechooser_button_clicked(GtkButton *, GtkWidget *);

static void cb_treeview_cursor_changed(GtkTreeView *, gpointer);

enum {
    COLUMN_NAME = 0,
    COLUMN_WIDGET,
    N_COLUMN
};

UimConfigContainer *uim_config_container_new(void){
    UimConfigContainer	*container;

    GtkWidget *notebook, *vbox1, *hbuttonbox;
    GtkWidget *button_save, *separator;
    GtkWidget *table, *lbl;
    gint i = 0;
    gint ret;
    /* uim_lisp groups; */

    ret = init_uim_config(&uim_config);
    uimapi_gc_protect(&groups);
    uimapi_gc_protect(&group_sym);
    uimapi_gc_protect(&cur);
    uimapi_gc_protect(&item);
    uimapi_gc_protect(&label);
    uimapi_gc_protect(&default_val);
    uimapi_gc_protect(&desc);
    uimapi_gc_protect(&range);
    uimapi_gc_protect(&group);
    uimapi_gc_protect(&customs);
    uimapi_gc_protect(&custom_sym);
    uimapi_gc_protect(&rcustoms);

    /* I changed coding style a little. */
    container = g_malloc0(sizeof(UimConfigContainer));

    vbox1 = gtk_vbox_new(FALSE, 5);
    gtk_container_set_border_width(GTK_CONTAINER(vbox1), 10);

    notebook = gtk_notebook_new();
    gtk_widget_show(notebook);

    gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_TOP);
    gtk_container_set_border_width(GTK_CONTAINER(notebook), 0);

    gtk_notebook_set_scrollable(GTK_NOTEBOOK(notebook), TRUE);
    gtk_notebook_set_show_tabs(GTK_NOTEBOOK(notebook), FALSE);
    gtk_notebook_set_show_border(GTK_NOTEBOOK(notebook), FALSE);
    {
	GtkWidget *paned;
	GtkWidget *scrollwin, *treeview;
	GtkTreeStore *store;
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;
	GList *node;

	paned = gtk_hpaned_new();
	gtk_widget_show(paned);
	gtk_container_set_border_width(GTK_CONTAINER(paned), 5);
	gtk_box_pack_start(GTK_BOX(vbox1), paned, TRUE, TRUE, 0);

	scrollwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrollwin),
                                       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrollwin),
					    GTK_SHADOW_IN);
	gtk_widget_set_size_request(scrollwin, 170, -1);
        gtk_widget_show(scrollwin);

	gtk_paned_add1(GTK_PANED(paned), scrollwin);
	gtk_paned_add2(GTK_PANED(paned), notebook);

	store = gtk_tree_store_new(N_COLUMN,
				   G_TYPE_STRING,
				   G_TYPE_POINTER);

	treeview = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(treeview), TRUE);
	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(treeview), FALSE);

	g_signal_connect(G_OBJECT(treeview), "cursor_changed",
			 G_CALLBACK(cb_treeview_cursor_changed),
			 container);

	gtk_container_add(GTK_CONTAINER(scrollwin), treeview);
	gtk_widget_show(treeview);

	column = gtk_tree_view_column_new();

	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_add_attribute(column, renderer,
					   "text", COLUMN_NAME);
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview), column);
	gtk_tree_view_set_expander_column(GTK_TREE_VIEW(treeview), column);

	gtk_tree_store_clear(store);

	groups = uim_custom_list_primary_groups();

	while(!uim_scm_nullp(groups))
	{
	    char *group_str;
	    /* uim_lisp group_sym; */
	    GtkTreeIter iter;
	
	    group_sym = uim_scm_car(groups);
	
	    if(!uim_scm_nullp(group_sym)) {
		group_str = uim_scm_c_str(group_sym);
		fprintf(stderr, "group: %s\n", group_str);

		table = uimconfig_uim_custom_widget_new_by_sym(group_sym);
		gtk_widget_show(table);
		gtk_container_add(GTK_CONTAINER(notebook), table);
	    
		lbl = gtk_label_new(group_str);
		gtk_widget_show(lbl);
		gtk_notebook_set_tab_label(GTK_NOTEBOOK(notebook),
					   gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook), i),
					   lbl);

		gtk_tree_store_append(store, &iter, NULL);
		gtk_tree_store_set(store, &iter,
				   COLUMN_NAME, group_str,
				   COLUMN_WIDGET, table,
				   -1);
		i++;
	    }

	    groups = uim_scm_cdr(groups);
	}
    }

    separator = gtk_hseparator_new();
    gtk_widget_show(separator);
    gtk_box_pack_start(GTK_BOX(vbox1), separator, FALSE, FALSE, 0);

    hbuttonbox = gtk_hbutton_box_new();
    gtk_button_box_set_layout(GTK_BUTTON_BOX(hbuttonbox), GTK_BUTTONBOX_END);
    gtk_box_set_spacing(GTK_BOX(hbuttonbox), 10);
    gtk_widget_show(hbuttonbox);
    gtk_box_pack_start(GTK_BOX(vbox1), hbuttonbox, FALSE, FALSE, 10);

    button_save = gtk_button_new_with_label(_("Apply"));
    gtk_widget_show(button_save);
    gtk_box_pack_start(GTK_BOX(hbuttonbox), button_save, FALSE, FALSE, 10);

    g_signal_connect(G_OBJECT(button_save), "clicked",
		     G_CALLBACK(cb_uim_config_pane_save_button_clicked),
		     container);

    container->container = vbox1;
    container->notebook = notebook;
    container->button_save = button_save;

    return container;
}

static void uimconfig_create_custom_combobox_widget(GtkWidget *table,
						    int row_nth,
						    uim_lisp sym)
{
    int num = 0, i = 0;
    int row, column;
    /*
    uim_lisp cur = NULL;
    uim_lisp label, default_val, desc;
    */
    GtkWidget *combobox_widget, *label_widget;
    char *label_str = NULL;

    g_object_get(G_OBJECT(table), "n_rows", &row, NULL);
    g_object_get(G_OBJECT(table), "n_columns", &column, NULL);

    if(row_nth > row) {
	g_print("row: %d, nth: %d\n", row, row_nth);
	gtk_table_resize(GTK_TABLE(table), row_nth, column);
    }

    label = uim_custom_label(sym);
    desc  = uim_custom_desc(sym);
    default_val = uim_custom_default_value(sym);

    label_str = uim_scm_c_str(label);
    g_print("label_str: %s\n", label_str);

    label_widget = gtk_label_new(label_str);
    gtk_misc_set_alignment(GTK_MISC(label_widget), 1.0, 0.5);
    gtk_widget_show(label_widget);
    gtk_table_attach(GTK_TABLE(table), label_widget,
		     0, 1, row_nth - 1, row_nth, GTK_FILL, 0, 5, 5);

    num = 0;
    combobox_widget = gtk_combo_box_new_text();
    gtk_widget_show(combobox_widget);

    cur = uim_custom_range(sym); i = 0;
    while(!uim_scm_nullp(cur))
    {
	char *lbl_str = NULL;
	/* uim_lisp item = NULL; */

	item = uim_scm_car(cur);
	if(!uim_scm_nullp(item)) {
	    lbl_str = uim_custom_symbol_label(sym, item);

	    if(lbl_str != NULL)
	    {
		g_print("lbl_str: %s\n", lbl_str);

		gtk_combo_box_append_text(GTK_COMBO_BOX(combobox_widget), lbl_str);
		if(uim_scm_eq(item, uim_custom_value(sym)))
		{
		    num = i;
		}
		
		free(lbl_str);
	    }
	    i++;
	}
	cur = uim_scm_cdr(cur);
    }

    gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_widget), num);

    gtk_table_attach(GTK_TABLE(table), combobox_widget,
		     1, 2, row_nth - 1, row_nth, GTK_FILL, 0, 5, 5);

    custom_widget_list = uim_custom_widgetlist_append(custom_widget_list,
						      UCustom_Symbol,
						      combobox_widget, sym);

    if(label_str != NULL)
	free(label_str);
}


static void uimconfig_create_custom_checkbutton_widget(GtkWidget *table,
						       int row_nth,
						       uim_lisp sym)
{
    GtkWidget *checkbutton;
    /* uim_lisp label, default_val, desc; */
    int row = 0, column = 0;
    char *label_str = NULL;
    Uim_CustomWidgetList list;

    g_object_get(G_OBJECT(table), "n_rows", &row, NULL);
    g_object_get(G_OBJECT(table), "n_columns", &column, NULL);

    label = uim_custom_label(sym);
    desc  = uim_custom_desc(sym);
    default_val = uim_custom_default_value(sym);

    if(row_nth > row) {
	g_print("row: %d, nth: %d\n", row, row_nth);
	gtk_table_resize(GTK_TABLE(table), row_nth, column);
    }

    label_str = uim_scm_c_str(label);
    g_print("label_str: %s\n", label_str);

    checkbutton = gtk_check_button_new_with_label(label_str);
    gtk_widget_show(checkbutton);

    gtk_table_attach(GTK_TABLE(table), checkbutton,
		     0, 1, row_nth - 1, row_nth, GTK_FILL, 0, 5, 5);

    if(uim_custom_value_as_bool(sym))
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton), TRUE);

    /* TODO: must handle signal to each widget when update(apply button)*/

    custom_widget_list = uim_custom_widgetlist_append(custom_widget_list,
						      UCustom_Bool,
						      checkbutton, sym);

    if(label_str != NULL)
	free(label_str);
}

static void uimconfig_create_custom_textbox_widget(GtkWidget *table,
						   int row_nth,
						   uim_lisp sym)
{
    GtkWidget *label_widget, *textbox_widget;
    /* uim_lisp label, default_val, desc; */
    int row = 0, column = 0;
    char *label_str = NULL, *default_str = NULL, *val_str = NULL;

    g_object_get(G_OBJECT(table), "n_rows", &row, NULL);
    g_object_get(G_OBJECT(table), "n_columns", &column, NULL);

    label = uim_custom_label(sym);
    desc  = uim_custom_desc(sym);
    default_val = uim_custom_default_value(sym);
    if(row_nth > row) {
	g_print("row: %d, nth: %d\n", row, row_nth);
	gtk_table_resize(GTK_TABLE(table), row_nth, column);
    }

    label_str = uim_scm_c_str(label);
    val_str = uim_custom_value_as_str(sym);

    g_print("label_str: %s\n", label_str);
    label_widget = gtk_label_new(label_str);
    gtk_misc_set_alignment(GTK_MISC(label_widget), 1.0, 0.5);
    gtk_widget_show(label_widget);
    gtk_table_attach(GTK_TABLE(table), label_widget,
		     0, 1, row_nth - 1, row_nth, GTK_FILL, 0, 5, 5);

    textbox_widget = gtk_entry_new();
    gtk_widget_show(textbox_widget);
    gtk_table_attach(GTK_TABLE(table), textbox_widget,
		     1, 2, row_nth - 1, row_nth, GTK_FILL, 0, 5, 5);
    if(val_str != NULL) {
	gtk_entry_set_text(GTK_ENTRY(textbox_widget), val_str);
	free(val_str);
    }

    custom_widget_list = uim_custom_widgetlist_append(custom_widget_list,
						      UCustom_Str,
						      textbox_widget, sym);

    if(label_str != NULL)
	free(label_str);
}


static void uimconfig_create_custom_spinbutton_widget(GtkWidget *table,
						      int row_nth,
						      uim_lisp sym)
{
    GtkWidget *label_widget, *spinbutton_widget;
    GtkAdjustment *adjustment;
    /* uim_lisp label, default_val, desc, range; */
    int val_max = 100, val_min = 0, val;
    int row = 0, column = 0;
    char *label_str = NULL, *default_str = NULL;

    g_object_get(G_OBJECT(table), "n_rows", &row, NULL);
    g_object_get(G_OBJECT(table), "n_columns", &column, NULL);

    label = uim_custom_label(sym);
    desc  = uim_custom_desc(sym);
    default_val = uim_custom_default_value(sym);

    if(row_nth > row) {
	g_print("row: %d, nth: %d\n", row, row_nth);
	gtk_table_resize(GTK_TABLE(table), row_nth, column);
    }

    label_str = uim_scm_c_str(label);
    val = uim_custom_value_as_int(sym);
    range = uim_custom_range(sym);
    val_min = uim_scm_c_int(uim_scm_car(range));
    val_max = uim_scm_c_int(uim_scm_cadr(range));

    label_widget = gtk_label_new(label_str);
    gtk_misc_set_alignment(GTK_MISC(label_widget), 1.0, 0.5);
    gtk_widget_show(label_widget);
    gtk_table_attach(GTK_TABLE(table), label_widget,
		     0, 1, row_nth - 1, row_nth, GTK_FILL, 0, 5, 5);

    /* XXX: only handle integer */
    adjustment = (GtkAdjustment*)gtk_adjustment_new((double)val,
						    (double)val_min,
						    (double)val_max,
						    1.0, 100.0, 100.0);
    spinbutton_widget = gtk_spin_button_new(adjustment, 1.0, 0);
    gtk_widget_show(spinbutton_widget);
    gtk_table_attach(GTK_TABLE(table), spinbutton_widget,
		     1, 2, row_nth - 1, row_nth, GTK_FILL, 0, 5, 5);

    custom_widget_list = uim_custom_widgetlist_append(custom_widget_list,
						      UCustom_Int,
						      spinbutton_widget, sym);

    if(label_str != NULL)
	free(label_str);
}

static
void cb_uim_config_pane_save_button_clicked(GtkButton *button,
					    UimConfigContainer *container)
{
    GList *pos;
    struct _uim_config config;

    for(pos = g_list_first(custom_widget_list);	pos != NULL;
	pos = g_list_next(pos))
    {
	Uim_CustomWidgetList *data;
	uim_lisp sym;
	
	data = pos->data;

	switch(data->type) {
	case UCustom_Bool:
	{
	    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(data->widget)) == TRUE)
		uim_custom_set(uim_scm_intern_c_str(data->symbol_str),
			       uim_scm_t());
	    else
		uim_custom_set(uim_scm_intern_c_str(data->symbol_str),
			       uim_scm_f());
	    break;
	}
	case UCustom_Int:
	{
	    guint64 value;
	    value = g_ascii_strtoull(gtk_entry_get_text(GTK_ENTRY(data->widget)),
				     NULL,
				     10);
	    uim_custom_set(uim_scm_intern_c_str(data->symbol_str),
			   uim_scm_int_from_c_int((int)value));
	    break;
	}
	case UCustom_Str:
	{
	    gchar *value;

	    value = gtk_entry_get_text(GTK_ENTRY(data->widget));

	    uim_custom_set(uim_scm_intern_c_str(data->symbol_str),
			   uim_scm_str_from_c_str(value));
	    break;
	}
	case UCustom_Path:
	{
	    gchar *value;

	    value = gtk_entry_get_text(GTK_ENTRY(data->widget));

	    uim_custom_set(uim_scm_intern_c_str(data->symbol_str),
			   uim_scm_str_from_c_str(value));
	    break;
	}
	case UCustom_Symbol:
	{
	    /* XXX */
	    int i;

	    i = gtk_combo_box_get_active(GTK_COMBO_BOX(data->widget));
	    cur = uim_scm_intern_c_str(data->symbol_str);

	    item = uim_scm_nth(uim_scm_int_from_c_int(i),
			       uim_scm_quote(uim_custom_range(cur)));

	    uim_custom_set(cur, uim_scm_quote(item));

	    break;
	}
	default:
	    break;
	}
    }
    write_uim_config(&config);
}

static void uimconfig_create_custom_filechooser_widget(GtkWidget *table,
						       int row_nth,
						       uim_lisp sym)
{
    GtkWidget *label_widget, *textbox_widget, *button_widget;
    GtkWidget *hbox;
    /* uim_lisp label, default_val, desc; */
    int row = 0, column = 0;
    char *label_str = NULL, *default_str = NULL, *val = NULL;

    g_object_get(G_OBJECT(table), "n_rows", &row, NULL);
    g_object_get(G_OBJECT(table), "n_columns", &column, NULL);

    label = uim_custom_label(sym);
    desc  = uim_custom_desc(sym);
    default_val = uim_custom_default_value(sym);

    if(row_nth > row) {
	g_print("row: %d, nth: %d\n", row, row_nth);
	gtk_table_resize(GTK_TABLE(table), row_nth, column);
    }

    label_str = uim_scm_c_str(label);
    val = uim_custom_value_as_path(sym);

    g_print("label_str: %s\n", label_str);

    label_widget = gtk_label_new(label_str);
    gtk_misc_set_alignment(GTK_MISC(label_widget), 1.0, 0.5);
    gtk_widget_show(label_widget);
    gtk_table_attach(GTK_TABLE(table), label_widget,
		     0, 1, row_nth - 1, row_nth, GTK_FILL, 0, 5, 5);

    hbox = gtk_hbox_new(FALSE, 5);
    gtk_widget_show(hbox);
    gtk_table_attach(GTK_TABLE(table), hbox,
		     1, 2, row_nth - 1, row_nth, GTK_FILL, 0, 5, 5);

    textbox_widget = gtk_entry_new();
    gtk_widget_show(textbox_widget);
    gtk_box_pack_start(GTK_BOX(hbox), textbox_widget, FALSE, FALSE, 0);
    if(val != NULL)
	gtk_entry_set_text(GTK_ENTRY(textbox_widget), val);

    button_widget = gtk_button_new_with_label(_("Browse..."));
    gtk_widget_show(button_widget);
    gtk_box_pack_start(GTK_BOX(hbox), button_widget, FALSE, FALSE, 0);
    g_signal_connect(G_OBJECT(button_widget), "clicked",
		     G_CALLBACK(cb_uim_custom_filechooser_button_clicked),
		     textbox_widget);

    custom_widget_list = uim_custom_widgetlist_append(custom_widget_list,
						      UCustom_Path,
						      textbox_widget, sym);

    if(label_str != NULL)
	free(label_str);
    if(val != NULL)
	free(val);
}

void cb_uim_custom_filechooser_button_clicked(GtkButton *button,
					      GtkWidget *text_widget)
{
    GtkWidget *dialog, *toplevel;

    toplevel = gtk_widget_get_toplevel(GTK_WIDGET(button));
    dialog = gtk_file_chooser_dialog_new(_("Open File"),
					 GTK_WINDOW(toplevel),
					 GTK_FILE_CHOOSER_ACTION_OPEN,
					 GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					 GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
					 NULL);

    if(gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
	char *filename;

	filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));

	if(filename != NULL) {
	    gtk_entry_set_text(GTK_ENTRY(text_widget), filename);
	    free(filename);
	}
    }

    gtk_widget_destroy(dialog);
}

static GtkWidget* uimconfig_uim_custom_widget_new(const char *group_str)
{
    GtkWidget *table;
    /* uim_lisp group = NULL, customs = NULL, custom_sym = NULL, rcustoms = NULL; */
    int i = 1;

    g_return_val_if_fail(group_str, NULL);

    /* extract group */
    group = uim_scm_intern_c_str(group_str);
    if(uim_scm_nullp(group))
	return NULL;

    customs = uim_custom_collect_by_group(group);
    if(uim_scm_nullp(customs))
	return NULL;
    rcustoms = uim_scm_reverse(customs);

    table = gtk_table_new(1, 2, FALSE);

    while(!uim_scm_nullp(rcustoms))
    {
	int custom_ctype;

	custom_sym   = uim_scm_car(rcustoms);
	if(!uim_scm_nullp(custom_sym)) {
	    custom_ctype = uim_custom_ctype(custom_sym);

	    switch(custom_ctype) {
	    case UCustom_Bool:
		/* create check box widget */
		uimconfig_create_custom_checkbutton_widget(table, i, custom_sym);
		i++;
		break;
	    case UCustom_Int:
		/* create spinbutton widget */
		uimconfig_create_custom_spinbutton_widget(table, i, custom_sym);
		i++;
		break;
	    case UCustom_Str:
		/* create text widget */
		uimconfig_create_custom_textbox_widget(table, i, custom_sym);
		i++;
		break;
	    case UCustom_Path:
		/* create textbox and filechooser widget */
		uimconfig_create_custom_filechooser_widget(table, i, custom_sym);
		i++;
		break;
	    case UCustom_Symbol:
		uimconfig_create_custom_combobox_widget(table, i, custom_sym);
		i++;
		break;
/*
	    case UCustom_:
		uimconfig_create_custom_combobox_widget(table, i, custom_sym);
		i++;
*/
	    default:
		break;
	    }
	}

	rcustoms = uim_scm_cdr(rcustoms);
    }

    return table;
}

static GtkWidget* uimconfig_uim_custom_widget_new_by_sym(const uim_lisp group)
{
    GtkWidget *table;
    /* uim_lisp customs = NULL, custom_sym = NULL, rcustoms = NULL; */
    int i = 1;

    /* extract group */
    if(uim_scm_nullp(group))
	return NULL;

    customs = uim_custom_collect_by_group(group);
    if(uim_scm_nullp(customs))
	return NULL;

    rcustoms = uim_scm_reverse(customs);

    table = gtk_table_new(1, 2, FALSE);

    while(!uim_scm_nullp(rcustoms))
    {
	int custom_ctype;

	custom_sym   = uim_scm_car(rcustoms);
	if(!uim_scm_nullp(custom_sym))
	{
	    custom_ctype = uim_custom_ctype(custom_sym);

	    switch(custom_ctype) {
	    case UCustom_Bool:
		/* create check box widget */
		uimconfig_create_custom_checkbutton_widget(table, i, custom_sym);
		i++;
		break;
	    case UCustom_Int:
		/* create spinbutton widget */
		uimconfig_create_custom_spinbutton_widget(table, i, custom_sym);
		i++;
		break;
	    case UCustom_Str:
		/* create text widget */
		uimconfig_create_custom_textbox_widget(table, i, custom_sym);
		i++;
		break;
	    case UCustom_Path:
		/* create textbox and filechooser widget */
		uimconfig_create_custom_filechooser_widget(table, i, custom_sym);
		i++;
		break;
	    case UCustom_Symbol:
		uimconfig_create_custom_combobox_widget(table, i, custom_sym);
		i++;
		break;
/*
	case UCustom_:
	    uimconfig_create_custom_combobox_widget(table, i, custom_sym);
	    i++;
*/
	    default:
		break;
	    }
	}

	rcustoms = uim_scm_cdr(rcustoms);
    }

    return table;
}

static void cb_treeview_cursor_changed(GtkTreeView *treeview, gpointer data)
{
    UimConfigContainer *container;
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeIter iter;
    GtkWidget *widget;
    int num;

    container = (UimConfigContainer*)data;

    selection = gtk_tree_view_get_selection(treeview);
    gtk_tree_selection_get_selected(selection, &model, &iter);

    gtk_tree_model_get(model, &iter,
		       COLUMN_WIDGET, &widget,
		       -1);

    num = gtk_notebook_page_num(GTK_NOTEBOOK(container->notebook), widget);

    if(num >= 0)
	gtk_notebook_set_current_page(GTK_NOTEBOOK(container->notebook), num);
}

GList *uim_custom_widgetlist_append(GList *list,
				    enum UCustomType type,
				    GtkWidget *widget,
				    uim_lisp sym)
{
    char *custom_sym_str;
    Uim_CustomWidgetList *data;

    data = g_new0(Uim_CustomWidgetList, 1);

    custom_sym_str = uim_scm_c_str(sym);

    data->type   = type;
    data->widget = widget;
    data->symbol_str = custom_sym_str;
    data->group_str = NULL; /* XXX */

    list = g_list_append(list, data);

    return list;
}
