include libtool-version.mk
include $(top_builddir)/sigscheme/libgcroots.mk

AUTOMAKE_OPTIONS = foreign

EXTRA_DIST = uim-compat-scm.c encoding-table.c test-gc.c

uim_defs = -DSCM_FILES=\"$(datadir)/uim\"
# FIXME: $(UIM_SCM_CFLAGS) should only affect on uim-scm.c
CFLAGS = @CFLAGS@ $(UIM_SCM_CFLAGS)

lib_LTLIBRARIES = libuim.la libuim-custom.la

# Although uim only needs *.so for the plugins, whether *.la is
# installed or not is system-dependent and uim leaves it untouched. It
# will installed with normal libtool, or may not installed depending
# on the ltmain the system uses. For example, FreeBSD's libtool does
# not. Likewise it, *.a is also untouched other than libtool's
# package-global 'static' configuration since no partial disabling of
# static libraaries is supported by libtool. And no violent rm of *.a
# and *.la is performed to avoid system-dependent unexpected
# result. It's libtool's responsibility.  -- YamaKen 2006-05-20
uim_plugin_LTLIBRARIES = 
uim_plugindir = $(pkglibdir)/plugin

BUILT_SOURCES = sigscheme-combined
.PHONY: sigscheme-combined
sigscheme-combined:
	$(MAKE) $(AM_MAKEFLAGS) -C $(top_builddir)/sigscheme/src combined

pkginclude_HEADERS = uim.h uim-util.h uim-helper.h uim-im-switcher.h \
		uim-scm.h uim-scm-abbrev.h uim-custom.h plugin.h

libuim_la_SOURCES = uim.c uim-util.c uim-func.c uim-key.c \
		uim-internal.h gettext.h \
		uim-scm.c \
		uim-helper.c uim-helper-client.c \
		iconv.c intl.c \
		uim-ipc.c \
		plugin.c

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
if ENABLE_ANTHY_STATIC
  libuim_la_SOURCES += anthy.c
else
  uim_plugin_LTLIBRARIES += libuim-anthy.la
  libuim_anthy_la_SOURCES = anthy.c
  libuim_anthy_la_LIBADD = @ANTHY_LIBS@ libuim.la
  libuim_anthy_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
  libuim_anthy_la_CPPFLAGS = -I$(top_srcdir)
endif
endif

if MANA
uim_plugin_LTLIBRARIES += libuim-mana.la
libuim_mana_la_SOURCES = mana.c
libuim_mana_la_LIBADD = libuim.la
libuim_mana_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_mana_la_CPPFLAGS = -I$(top_srcdir)
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
libuim_skk_la_LIBADD = libuim.la @NETLIBS@
libuim_skk_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_skk_la_CPPFLAGS = -I$(top_srcdir)

libuimincludedir =  $(includedir)/uim

# NOTE: Don't depend on -export-symbols-regex to avoid symbol
# conflict. The libtool option is not and will not be supported on
# some platforms. See [Anthy-dev 2847].  -- YamaKen 2006-03-30
libuim_la_LDFLAGS = -version-info $(libuim_version) -export-dynamic
libuim_la_LIBADD = $(GCROOTS_LIBS) \
		   @LTLIBINTL@ @LTLIBICONV@ \
		   $(top_builddir)/replace/libreplace.la
# place -I$(top_srcdir) surely prior to sigscheme dirs
libuim_la_CPPFLAGS = $(uim_defs) \
		     -I$(top_srcdir) \
		     -I$(top_builddir)/sigscheme/include \
		     -I$(top_srcdir)/sigscheme/src \
		     -I$(top_builddir)/sigscheme/src \
		     -DPKGLIBDIR=\"$(pkglibdir)\" \
		     -DPKGDATADIR=\"$(pkgdatadir)\"
libuim_la_CFLAGS = $(GCROOTS_CFLAGS) $(UIM_SCM_CFLAGS)

if ENABLE_ANTHY_STATIC
  libuim_la_LIBADD += @ANTHY_LIBS@
  libuim_la_CPPFLAGS += -DENABLE_ANTHY_STATIC
endif

if DEBUG
libuim_la_CPPFLAGS += -DDEBUG -DDEBUG_SCM=1
endif

libuim_custom_la_LDFLAGS = -version-info $(libuim_custom_version) \
			   -export-dynamic
libuim_custom_la_LIBADD = libuim.la
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
uim_helper_server_LDADD = $(top_builddir)/replace/libreplace.la

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
uim_module_manager_LDADD = libuim.la
uim_module_manager_SOURCES = uim-module-manager.c

noinst_PROGRAMS = uim-agent

uim_agent_SOURCES = agent.c
uim_agent_LDADD   = libuim.la

DISTCLEANFILES = uim-stdint.h
