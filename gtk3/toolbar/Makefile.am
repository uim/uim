if GTK3
bin_PROGRAMS = uim-toolbar-gtk3 uim-toolbar-gtk3-systray
uim_toolbar_gtk3_SOURCES  = ../../helper/toolbar-standalone-gtk.c \
			       ../../helper/toolbar-common-gtk.c
uim_toolbar_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la \
			   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk3_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_toolbar_gtk3_CFLAGS   = @GTK3_CFLAGS@

uim_toolbar_gtk3_systray_SOURCES  = ../../helper/toolbar-systray-gtk.c \
				   ../../helper/toolbar-common-gtk.c \
				   ../../helper/eggtrayicon.c ../../helper/eggtrayicon.h
uim_toolbar_gtk3_systray_LDADD    = @GTK3_LIBS@ \
				   @X11_LIBS@ \
				   $(top_builddir)/uim/libuim-scm.la \
				   $(top_builddir)/uim/libuim.la \
				   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk3_systray_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_toolbar_gtk3_systray_CFLAGS   = @GTK3_CFLAGS@
endif
