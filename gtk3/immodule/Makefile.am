uim_gtk3_im_module_path = $(libdir)/gtk-3.0

moduledir = $(uim_gtk3_im_module_path)/@GTK3_BINARY_VERSION@/immodules

if GTK3
im_uim_la = im-uim.la
im_uim_la_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
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
	../../gtk2/immodule/gtk-im-uim.c ../../gtk2/immodule/gtk-im-uim.h \
	../../gtk2/immodule/key-util-gtk.c ../../gtk2/immodule/key-util-gtk.h \
	../../gtk2/immodule/uim-cand-win-gtk.c \
	../../gtk2/immodule/uim-cand-win-gtk.h \
	../../gtk2/immodule/uim-cand-win-vertical-gtk.c \
	../../gtk2/immodule/uim-cand-win-vertical-gtk.h \
	../../gtk2/immodule/uim-cand-win-tbl-gtk.c \
	../../gtk2/immodule/uim-cand-win-tbl-gtk.h \
	../../gtk2/immodule/uim-cand-win-horizontal-gtk.c \
	../../gtk2/immodule/uim-cand-win-horizontal-gtk.h \
	../../gtk2/immodule/caret-state-indicator.c \
	../../gtk2/immodule/caret-state-indicator.h \
	../../gtk2/immodule/compose.c ../../gtk2/immodule/compose.h \
	../../gtk2/immodule/text-util.c ../../gtk2/immodule/text-util.h

QUERY_COMMAND = gtk-query-immodules-3.0

install-data-hook:
	if test -z $(DESTDIR); then \
	  if test $(libdir) = $(GTK3_LIBDIR); then \
	    if type $(QUERY_COMMAND) > /dev/null 2>&1; then \
	      $(QUERY_COMMAND) --update-cache; \
	    else \
	      echo "********************** Warning ***********************"; \
	      echo " $(QUERY_COMMAND) not found"; \
	      echo "******************************************************"; \
	    fi \
	  else \
	    if type $(QUERY_COMMAND) > /dev/null 2>&1; then \
	      GTK_PATH=$(uim_gtk3_im_module_path) $(QUERY_COMMAND) --update-cache; \
	    else \
	      echo "********************** Warning ***********************"; \
	      echo " $(QUERY_COMMAND) not found"; \
	      echo " Please make sure to update"; \
	      echo " immodules.cache"; \
	      echo " manually, and set"; \
	      echo " GTK_IM_MODULE_FILE=PATH_TO/immodule.cache"; \
	      echo " environment variable to use this module."; \
	      echo "******************************************************"; \
	    fi \
	  fi \
	fi
uninstall-hook:
	if test -z $(DESTDIR); then \
	  if type $(QUERY_COMMAND) > /dev/null 2>&1; then \
	    if test $(libdir) = $(GTK3_LIBDIR); then \
	      $(QUERY_COMMAND) --update-cache; \
	    else \
	      GTK_PATH=$(uim_gtk3_im_module_path) $(QUERY_COMMAND) --update-cache; \
	    fi \
	  fi \
	fi
else
install-data-hook:

uninstall-hook:

endif
