EXTRA_DIST = meson.build update-immodules-cache.sh

uim_gtk3_im_module_path = $(libdir)/gtk-3.0

moduledir = $(uim_gtk3_im_module_path)/@GTK3_BINARY_VERSION@/immodules

if GTK3
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

im_uim_la = im-uim.la
im_uim_la_CPPFLAGS = $(AM_CPPFLAGS)
im_uim_la_CFLAGS = @GTK3_CFLAGS@
module_LTLIBRARIES = $(im_uim_la)


im_uim_la_SOURCES = $(IM_UIM_SOURCES)
im_uim_la_LDFLAGS = -module -avoid-version
im_uim_la_LIBADD = $(top_builddir)/uim/libuim-scm.la \
		   $(top_builddir)/uim/libuim.la \
		   $(top_builddir)/uim/libuim-counted-init.la
if LIBUIM_X_UTIL
im_uim_la_LIBADD += $(top_builddir)/uim/libuim-x-util.la
endif
im_uim_la_LIBADD += @GTK3_LIBS@
im_uim_la_LIBADD += -lm


IM_UIM_SOURCES = \
	gtk-im-uim.c gtk-im-uim.h \
	key-util-gtk.c key-util-gtk.h \
	uim-cand-win-gtk.c \
	uim-cand-win-gtk.h \
	uim-cand-win-vertical-gtk.c \
	uim-cand-win-vertical-gtk.h \
	uim-cand-win-tbl-gtk.c \
	uim-cand-win-tbl-gtk.h \
	uim-cand-win-horizontal-gtk.c \
	uim-cand-win-horizontal-gtk.h \
	caret-state-indicator.c \
	caret-state-indicator.h \
	compose.c compose.h \
	text-util.c text-util.h

QUERY_COMMAND = gtk-query-immodules-3.0

# GTK finds immodules in its libdir without GTK_PATH.
install-data-hook uninstall-hook:
	if test "$(libdir)" = "$(GTK3_LIBDIR)"; then \
	  gtk_path=; \
	else \
	  gtk_path="$(uim_gtk3_im_module_path)"; \
	fi; \
	DESTDIR="$(DESTDIR)" $(SHELL) $(srcdir)/update-immodules-cache.sh \
	  $(QUERY_COMMAND) "$$gtk_path"
else
install-data-hook:

uninstall-hook:

endif
