AUTOMAKE_OPTIONS = foreign

EXTRA_DIST = config.h.in iso-639-1.def

uim_defs = -DSCM_FILES=\"$(datadir)/uim\"

lib_LTLIBRARIES = libuim.la

pkginclude_HEADERS = uim.h uim-util.h uim-helper.h uim-im-switcher.h uim-scm.h

libuim_la_SOURCES = uim.c uim-func.c \
		uim-key.c uim-util.c  uim-helper.c uim-helper-client.c \
		skk-dic.c anthy.c \
		prime.c \
		slib.c siod.h context.h gettext.h \
		intl.c intl.h \
		uim-ipc.c uim-table.c \
		getpeereid.c \
		$(canna_source) $(m17nlib_source)

if CANNA
canna_source = canna.c
endif
if M17NLIB
m17nlib_source = m17nlib.c
endif

libuimincludedir =  $(includedir)/uim

libuim_la_LDFLAGS = -version-info 0:1:0 -export-symbols-regex uim.\*
libuim_la_LIBADD = @LIBINTL@ @LIBICONV@ @M17NLIB_LIBS@
libuim_la_CPPFLAGS = $(uim_defs) -I$(top_srcdir)
libuim_la_CFLAGS =

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
