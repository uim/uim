uim_gtk3_im_module_path = $(libdir)/gtk-3.0
uim_gtk3_im_module_file = $(DESTDIR)$(sysconfdir)/gtk-3.0/gtk.immodules

moduledir = $(uim_gtk3_im_module_path)/@GTK3_BINARY_VERSION@/immodules

if GTK3
im_uim_la = im-uim.la
im_uim_la_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
im_uim_la_CFLAGS = @GTK3_CFLAGS@
module_LTLIBRARIES = $(im_uim_la)


im_uim_la_SOURCES = $(IM_UIM_SOURCES)
im_uim_la_LDFLAGS = -module -avoid-version @GTK3_LIBS@
im_uim_la_LIBADD = $(top_builddir)/uim/libuim-scm.la \
		   $(top_builddir)/uim/libuim.la \
		   $(top_builddir)/uim/libuim-counted-init.la
if LIBUIM_X_UTIL
im_uim_la_LIBADD += $(top_builddir)/uim/libuim-x-util.la
endif


IM_UIM_SOURCES = \
	../../gtk/gtk-im-uim.c ../../gtk/gtk-im-uim.h \
	../../gtk/key-util-gtk.c ../../gtk/key-util-gtk.h \
	../../gtk/uim-cand-win-gtk.c ../../gtk/uim-cand-win-gtk.h \
	../../gtk/uim-cand-win-tbl-gtk.c ../../gtk/uim-cand-win-tbl-gtk.h \
	../../gtk/caret-state-indicator.c ../../gtk/caret-state-indicator.h \
	../../gtk/compose.c ../../gtk/compose.h \
	../../gtk/text-util.c ../../gtk/text-util.h

noinst_PROGRAMS = gtk3-rc-get-immodule-file
gtk3_rc_get_immodule_file_SOURCES = ../../gtk/gtk-rc-get-immodule-file.c
gtk3_rc_get_immodule_file_CFLAGS = @GTK3_CFLAGS@
gtk3_rc_get_immodule_file_LDADD = @GTK3_LIBS@
GTK3_RC_GET_IMMODULE_FILE = $(top_builddir)/gtk3/immodule/gtk3-rc-get-immodule-file
QUERY_COMMAND = gtk-query-immodules-3.0

install-data-hook: gtk3-rc-get-immodule-file
	if test -z $(DESTDIR); then \
	  if test $(libdir) = $(GTK3_LIBDIR); then \
	    if type $(QUERY_COMMAND) > /dev/null 2>&1; then \
	      $(QUERY_COMMAND) > `$(GTK3_RC_GET_IMMODULE_FILE)`; \
	      echo "*** \"`$(GTK3_RC_GET_IMMODULE_FILE)`\" is updated. ***";  \
	    else \
	      echo "********************** Warning ***********************"; \
	      echo " $(QUERY_COMMAND) not found"; \
	      echo " Please make sure to update"; \
	      echo " \"`$(GTK3_RC_GET_IMMODULE_FILE)`\""; \
	      echo " manually."; \
	      echo "******************************************************"; \
	    fi \
	  else \
	    if type $(QUERY_COMMAND) > /dev/null 2>&1; then \
	      $(mkinstalldirs) $(sysconfdir)/gtk-3.0; \
	      GTK3_PATH=$(uim_gtk3_im_module_path) $(QUERY_COMMAND) > $(uim_gtk3_im_module_file); \
	      echo "******************************************************"; \
	      echo " You need to set"; \
	      echo " GTK3_IM_MODULE_FILE=$(uim_gtk3_im_module_file)"; \
	      echo " environment variable to use this module."; \
	      echo "******************************************************"; \
	    else \
	      echo "********************** Warning ***********************"; \
	      echo " $(QUERY_COMMAND) not found"; \
	      echo " Please make sure to update"; \
	      echo " \"$(uim_gtk3_im_module_file)\""; \
	      echo " manually, and set"; \
	      echo " GTK3_IM_MODULE_FILE=$(uim_gtk3_im_module_file)"; \
	      echo " environment variable to use this module."; \
	      echo "******************************************************"; \
	    fi \
	  fi \
	fi
uninstall-hook:
	if test -z $(DESTDIR); then \
	  if type $(QUERY_COMMAND) > /dev/null 2>&1; then \
	    if test $(libdir) = $(GTK3_LIBDIR); then \
	      $(QUERY_COMMAND) > `$(GTK3_RC_GET_IMMODULE_FILE)`; \
	    else \
	      GTK3_PATH=$(uim_gtk3_im_module_path) $(QUERY_COMMAND) > $(uim_gtk3_im_module_file); \
	    fi \
	  fi \
	fi
else
install-data-hook:

uninstall-hook:

endif
