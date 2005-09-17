AUTOMAKE_OPTIONS = foreign

EXTRA_DIST = config.h.in iso-639-1.def slib.c uim-compat-scm.c

uim_defs = -DSCM_FILES=\"$(datadir)/uim\"

lib_LTLIBRARIES = libuim.la libuim-custom.la

uim_plugin_LTLIBRARIES = 
uim_plugindir = $(pkglibdir)/plugin

pkginclude_HEADERS = uim.h uim-util.h uim-helper.h uim-im-switcher.h \
		uim-scm.h uim-custom.h plugin.h

libuim_la_SOURCES = uim.c uim-scm.c uim-util.c uim-func.c uim-key.c \
		siod.h context.h gettext.h uim-encoding.h\
		uim-helper.c uim-helper-client.c \
		intl.c \
		uim-ipc.c \
		plugin.c
#		getpeereid.c

#if NEED_SETENV_C
#libuim_la_SOURCES += setenv.c
#endif
#if NEED_STRSEP_C
#libuim_la_SOURCES += strsep.c
#endif

if COMPAT_TABLE
libuim_la_SOURCES += uim-table.c
endif

if COMPAT_SCM
  pkginclude_HEADERS += uim-compat-scm.h
endif

libuim_custom_la_SOURCES = uim-custom.c

if M17NLIB
  uim_plugin_LTLIBRARIES += libuim-m17nlib.la
  libuim_m17nlib_la_SOURCES = m17nlib.c
  libuim_m17nlib_la_LIBADD = @M17NLIB_LIBS@ libuim.la
  libuim_m17nlib_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
  libuim_m17nlib_la_CPPFLAGS = -I$(top_srcdir)
endif

if SCIM
  uim_plugin_LTLIBRARIES += libuim-scim.la
  libuim_scim_la_SOURCES = scim.cpp
  libuim_scim_la_LIBADD = @SCIM_LIBS@ libuim.la
  libuim_scim_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
  libuim_scim_la_CXXFLAGS = -I$(top_srcdir) @SCIM_CFLAGS@
endif

if CANNA
  uim_plugin_LTLIBRARIES += libuim-canna.la
  libuim_canna_la_SOURCES = canna.c
  libuim_canna_la_LIBADD = libuim.la -lcanna
  libuim_canna_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
  libuim_canna_la_CPPFLAGS = -I$(top_srcdir)
endif

if ANTHY
  uim_plugin_LTLIBRARIES += libuim-anthy.la
  libuim_anthy_la_SOURCES = anthy.c
  libuim_anthy_la_LIBADD = libuim.la
  libuim_anthy_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
  libuim_anthy_la_CPPFLAGS = -I$(top_srcdir)
endif

if PRIME
  uim_plugin_LTLIBRARIES += libuim-prime.la
  libuim_prime_la_SOURCES = prime.c
  libuim_prime_la_LIBADD = libuim.la
  libuim_prime_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
  libuim_prime_la_CPPFLAGS = -I$(top_srcdir)
endif

uim_plugin_LTLIBRARIES += libuim-skk.la
libuim_skk_la_SOURCES = skk.c
libuim_skk_la_LIBADD = libuim.la
libuim_skk_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_skk_la_CPPFLAGS = -I$(top_srcdir)

libuimincludedir =  $(includedir)/uim

libuim_la_LDFLAGS = -version-info 0:2:0 -export-symbols-regex uim.\*
libuim_la_LIBADD = @LTLIBINTL@ @LTLIBICONV@ $(top_builddir)/replace/libreplace.la
libuim_la_CPPFLAGS = $(uim_defs) -I$(top_srcdir) -DPKGLIBDIR=\"$(pkglibdir)\" \
		     -DPKGDATADIR=\"$(pkgdatadir)\"

if DEBUG
libuim_la_CPPFLAGS += -DDEBUG -DDEBUG_SCM=1
endif

libuim_custom_la_LDFLAGS = -version-info 0:1:0 -export-symbols-regex uim.\*
libuim_custom_la_LIBADD = 
libuim_custom_la_CPPFLAGS = $(uim_defs) -I$(top_srcdir)

uim_plugin_LTLIBRARIES += libuim-custom-enabler.la
libuim_custom_enabler_la_SOURCES = uim-custom-enabler.c
libuim_custom_enabler_la_LIBADD = libuim-custom.la libuim.la
libuim_custom_enabler_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_custom_enabler_la_CPPFLAGS = -I$(top_srcdir)


bin_PROGRAMS = uim-sh uim-module-manager
libexec_PROGRAMS = uim-helper-server

uim_helper_server_LIBS =  
uim_helper_server_CPPFLAGS = $(uim_defs) -I$(top_srcdir)
uim_helper_server_CFLAGS =
uim_helper_server_SOURCES = uim-helper.c uim-helper-server.c

uim_sh_LIBS =
uim_sh_CPPFLAGS = $(uim_defs) -I$(top_srcdir)
uim_sh_CFLAGS =
uim_sh_LDADD = libuim.la
uim_sh_SOURCES = uim-sh.c
if LIBEDIT
  uim_sh_CFLAGS += -DLIBEDIT
  uim_sh_SOURCES += editline.c editline.h
  uim_sh_LDADD += -ledit -lcurses
endif

uim_module_manager_LIBS =
uim_module_manager_CPPFLAGS = $(uim_defs) -I$(top_srcdir) -DUIM_DATADIR=\""$(datadir)/uim"\"
uim_module_manager_CFLAGS =
uim_module_manager_LDADD = libuim.la $(top_builddir)/replace/libreplace.la
uim_module_manager_SOURCES = uim-module-manager.c
#if NEED_SETENV_C
#uim_module_manager_SOURCES += setenv.c
#endif

noinst_PROGRAMS = uim-agent

uim_agent_SOURCES = agent.c
uim_agent_LDADD   = libuim.la
