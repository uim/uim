EXTRA_DIST = test.sh.in

if GTK3
noinst_PROGRAMS = $(gtk3_entry)
gtk3_entry = gtk3-entry
gtk3_entry_SOURCES = entry.c
gtk3_entry_LDADD = @GTK3_LIBS@
gtk3_entry_CFLAGS = @GTK3_CFLAGS@
endif
