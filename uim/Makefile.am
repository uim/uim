AUTOMAKE_OPTIONS = foreign

EXTRA_DIST = config.h.in iso-639-1.def

uim_defs = -DSCM_FILES=\"$(datadir)/uim\"

lib_LTLIBRARIES = libuim.la

pkginclude_HEADERS = uim.h uim-util.h uim-helper.h uim-im-switcher.h uim-scm.h plugin.h

libuim_la_SOURCES = uim.c uim-scm.c uim-func.c uim-key.c uim-util.c \
		slib.c siod.h context.h gettext.h \
		uim-helper.c uim-helper-client.c \
		skk-dic.c anthy.c \
		prime.c \
		intl.c intl.h \
		uim-ipc.c uim-table.c \
		getpeereid.c \
		setenv.c \
		plugin.c

if COMPAT_SCM
  pkginclude_HEADERS += uim-compat-scm.h
  libuim_la_SOURCES += uim-compat-scm.c
endif

if COMPAT_CUSTOM
  pkginclude_HEADERS += uim-compat-custom.h
  libuim_la_SOURCES += uim-compat-custom.c
else
  pkginclude_HEADERS += uim-custom.h
  libuim_la_SOURCES += uim-custom.c
endif

if M17NLIB
  lib_LTLIBRARIES += libuim-m17nlib.la
  libuim_m17nlib_la_SOURCES = m17nlib.c
  libuim_m17nlib_la_LIBADD = @LIBINTL@ @LIBICONV@ @M17NLIB_LIBS@ libuim.la
  libuim_m17nlib_la_LDFLAGS = -luim
  libuim_m17nlib_la_CPPFLAGS = -I$(top_srcdir)
endif

if SCIM
  lib_LTLIBRARIES += libuim-scim.la
  libuim_scim_la_SOURCES = scim.cpp
  libuim_scim_la_LIBADD = @LIBINTL@ @LIBICONV@ @SCIM_LIBS@ libuim.la
  libuim_scim_la_LDFLAGS = -luim
  libuim_scim_la_CXXFLAGS = -I$(top_srcdir) @SCIM_CFLAGS@
endif

if CANNA
  lib_LTLIBRARIES += libuim-canna.la
  libuim_canna_la_SOURCES = canna.c
  libuim_canna_la_LIBADD = @LIBINTL@ @LIBICONV@ libuim.la
  libuim_canna_la_LDFLAGS = -luim
  libuim_canna_la_CPPFLAGS = -I$(top_srcdir)
endif

libuimincludedir =  $(includedir)/uim

libuim_la_LDFLAGS = -version-info 0:1:0 -export-symbols-regex uim.\*
libuim_la_LIBADD = @LIBINTL@ @LIBICONV@
libuim_la_CPPFLAGS = $(uim_defs) -I$(top_srcdir) -DLIBDIR=\"$(pkglibdir)\"

if DEBUG
libuim_la_CPPFLAGS += -DDEBUG -DDEBUG_SCM=1
endif

bin_PROGRAMS = uim-helper-server uim-sh

uim_helper_server_LIBS =  
uim_helper_server_CPPFLAGS = $(uim_defs) -I$(top_srcdir)
uim_helper_server_CFLAGS =
uim_helper_server_SOURCES = uim-helper.c uim-helper-server.c

uim_sh_LIBS =
uim_sh_CPPFLAGS = $(uim_defs) -I$(top_srcdir)
uim_sh_CFLAGS =
uim_sh_LDADD = libuim.la @LIBICONV@
uim_sh_SOURCES = uim-sh.c

noinst_PROGRAMS = uim-agent

uim_agent_SOURCES = agent.c
uim_agent_LDADD   = libuim.la @LIBICONV@
