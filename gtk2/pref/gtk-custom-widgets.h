/*

  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim

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

#include <glib.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define UIMPREF_FILE_ENTRY_TYPE	(uimpref_file_entry_get_type())
#define UIMPREF_FILE_ENTRY(obj)	(G_TYPE_CHECK_INSTANCE_CAST((obj), UIMPREF_FILE_ENTRY_TYPE, UimPrefFileEntry))
#define UIMPREF_FILE_ENTRY_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST((klass), UIMPREF_FILE_ENTRY_TYPE, UimPrefFileEntryClass))
#define IS_UIMPREF_FILE_ENTRY(obj)	(G_TYPE_CHECK_INSTANCE_TYPE((obj), UIMPREF_FILE_ENTRY_TYPE))
#define IS_UIMPREF_FILE_ENTRY_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE((klass), UIMPREF_FILE_ENTRY_TYPE))

typedef struct _UimPrefFileEntry	UimPrefFileEntry;
typedef struct _UimPrefFileEntryClass	UimPrefFileEntryClass;

struct _UimPrefFileEntryClass
{
  GtkEntryClass parent_class;
};

struct _UimPrefFileEntry
{
  GtkEntry entry;
  int type;
};

GType uimpref_file_entry_get_type(void);
GtkWidget *uimpref_file_entry_new(void);

G_END_DECLS


void uim_pref_gtk_add_custom(GtkWidget *vbox, const char *custom_sym);
void uim_pref_gtk_set_default_value(GtkWidget *widget);
