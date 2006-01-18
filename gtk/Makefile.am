SUBDIRS = test

moduledir = $(libdir)/gtk-2.0/@GTK_BINARY_VERSION@/immodules
GTK_IMMODULES= $(DESTDIR)$(sysconfdir)/gtk-2.0/gtk.immodules

if GTK2
im_uim_la = im-uim.la
im_uim_la_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
im_uim_la_CFLAGS = @GTK2_CFLAGS@ -Wall
module_LTLIBRARIES = $(im_uim_la)


im_uim_la_SOURCES = $(IM_UIM_SOURCES)
im_uim_la_LDFLAGS = -module -avoid-version @GTK2_LIBS@
im_uim_la_LIBADD = $(top_builddir)/uim/libuim.la $(EBLIB_LIBS)


IM_UIM_SOURCES = \
	gtk-im-uim.c key-util-gtk.c key-util-gtk.h \
	uim-cand-win-gtk.c uim-cand-win-gtk.h \
	caret-state-indicator.c caret-state-indicator.h

if EB
IM_UIM_SOURCES += uim-eb.c uim-eb.h
endif

install-data-hook:
	if test -f $(GTK_IMMODULES); then \
	gtk-query-immodules-2.0 > $(GTK_IMMODULES); \
	fi
uninstall-hook:
	if test -f $(GTK_IMMODULES); then \
	gtk-query-immodules-2.0 > $(GTK_IMMODULES); \
	fi
else
install-data-hook:

uninstall-hook:

endif
