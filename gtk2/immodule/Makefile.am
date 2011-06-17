uim_gtk_im_module_path = $(libdir)/gtk-2.0
uim_gtk_im_module_file = $(DESTDIR)$(sysconfdir)/gtk-2.0/gtk.immodules

moduledir = $(uim_gtk_im_module_path)/@GTK_BINARY_VERSION@/immodules

if GTK2
im_uim_la = im-uim.la
im_uim_la_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
im_uim_la_CFLAGS = @GTK2_CFLAGS@
module_LTLIBRARIES = $(im_uim_la)


im_uim_la_SOURCES = $(IM_UIM_SOURCES)
im_uim_la_LDFLAGS = -module -avoid-version @GTK2_LIBS@
im_uim_la_LIBADD = $(top_builddir)/uim/libuim-scm.la \
		   $(top_builddir)/uim/libuim.la \
		   $(top_builddir)/uim/libuim-counted-init.la
if LIBUIM_X_UTIL
im_uim_la_LIBADD += $(top_builddir)/uim/libuim-x-util.la
endif


IM_UIM_SOURCES = \
	gtk-im-uim.c gtk-im-uim.h key-util-gtk.c key-util-gtk.h \
	uim-cand-win-gtk.c uim-cand-win-gtk.h \
	uim-cand-win-tbl-gtk.c uim-cand-win-tbl-gtk.h \
	uim-cand-win-horizontal-gtk.c uim-cand-win-horizontal-gtk.h \
	caret-state-indicator.c caret-state-indicator.h \
	compose.c compose.h text-util.c text-util.h

noinst_PROGRAMS = gtk-rc-get-immodule-file
gtk_rc_get_immodule_file_SOURCES = gtk-rc-get-immodule-file.c
gtk_rc_get_immodule_file_CFLAGS = @GTK2_CFLAGS@
gtk_rc_get_immodule_file_LDADD = @GTK2_LIBS@
GTK_RC_GET_IMMODULE_FILE = $(top_builddir)/gtk2/immodule/gtk-rc-get-immodule-file
QUERY_COMMAND = gtk-query-immodules-2.0

install-data-hook: gtk-rc-get-immodule-file
	if test -z $(DESTDIR); then \
	  if test $(libdir) = $(GTK_LIBDIR); then \
	    if type $(QUERY_COMMAND) > /dev/null 2>&1; then \
	      $(QUERY_COMMAND) > `$(GTK_RC_GET_IMMODULE_FILE)`; \
	      echo "*** \"`$(GTK_RC_GET_IMMODULE_FILE)`\" is updated. ***";  \
	    else \
	      echo "********************** Warning ***********************"; \
	      echo " $(QUERY_COMMAND) not found"; \
	      echo " Please make sure to update"; \
	      echo " \"`$(GTK_RC_GET_IMMODULE_FILE)`\""; \
	      echo " manually."; \
	      echo "******************************************************"; \
	    fi \
	  else \
	    if type $(QUERY_COMMAND) > /dev/null 2>&1; then \
	      $(mkinstalldirs) $(sysconfdir)/gtk-2.0; \
	      GTK_PATH=$(uim_gtk_im_module_path) $(QUERY_COMMAND) > $(uim_gtk_im_module_file); \
	      echo "******************************************************"; \
	      echo " You need to set"; \
	      echo " GTK_IM_MODULE_FILE=$(uim_gtk_im_module_file)"; \
	      echo " environment variable to use this module."; \
	      echo "******************************************************"; \
	    else \
	      echo "********************** Warning ***********************"; \
	      echo " $(QUERY_COMMAND) not found"; \
	      echo " Please make sure to update"; \
	      echo " \"$(uim_gtk_im_module_file)\""; \
	      echo " manually, and set"; \
	      echo " GTK_IM_MODULE_FILE=$(uim_gtk_im_module_file)"; \
	      echo " environment variable to use this module."; \
	      echo "******************************************************"; \
	    fi \
	  fi \
	fi
uninstall-hook:
	if test -z $(DESTDIR); then \
	  if type $(QUERY_COMMAND) > /dev/null 2>&1; then \
	    if test $(libdir) = $(GTK_LIBDIR); then \
	      $(QUERY_COMMAND) > `$(GTK_RC_GET_IMMODULE_FILE)`; \
	    else \
	      GTK_PATH=$(uim_gtk_im_module_path) $(QUERY_COMMAND) > $(uim_gtk_im_module_file); \
	    fi \
	  fi \
	fi
else
install-data-hook:

uninstall-hook:

endif
