EXTRA_DIST = test.sh.in

if GTK2
noinst_PROGRAMS = $(gtk_entry)
gtk_entry = gtk-entry
gtk_entry_SOURCES = entry.c
gtk_entry_LDADD = @GTK2_LIBS@
gtk_entry_CFLAGS = @GTK2_CFLAGS@
endif
