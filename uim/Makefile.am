include libtool-version.mk
include $(top_builddir)/sigscheme/libgcroots.mk
include $(top_builddir)/sigscheme/sigscheme.mk

AUTOMAKE_OPTIONS = foreign

EXTRA_DIST = encoding-table.c test-gc.c

uim_defs = -DSCM_FILES=\"$(datadir)/uim\"
# FIXME: $(UIM_SCM_CFLAGS) should only affect on uim-scm.c
CFLAGS = @CFLAGS@ $(UIM_SCM_CFLAGS)
# FIXME: $(SCIM_CFLAGS) should only affect on scim.cpp
# 'if SCIM' makes this workaround broken
CXXFLAGS = @CXXFLAGS@ $(SCIM_CFLAGS)

lib_LTLIBRARIES = libuim-scm.la libuim.la libuim-custom.la
noinst_LTLIBRARIES = libuim-bsdlook.la
if LIBUIM_X_UTIL
noinst_LTLIBRARIES += libuim-x-util.la
endif
noinst_LTLIBRARIES += libuim-counted-init.la

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

pkginclude_HEADERS = uim-scm.h uim-scm-abbrev.h uim.h \
		uim-util.h uim-posix.h dynlib.h \
		uim-helper.h uim-im-switcher.h \
		uim-custom.h
# Install even if !NOTIFY since plugins will need this to use
# uim_notify_{info,fatal}().  -- YamaKen 2008-02-12
pkginclude_HEADERS += uim-notify.h

libuim_la_SOURCES = \
		uim-internal.h uim-error.c uim.c \
		uim-key.c uim-func.c uim-util.c uim-posix.c \
		uim-iconv.h iconv.c dynlib.c \
		uim-ipc.c uim-helper.c uim-helper-client.c \
		gettext.h intl.c \
		rk.c

uim_plugin_LTLIBRARIES += libuim-fileio.la
libuim_fileio_la_SOURCES = fileio.c
libuim_fileio_la_LIBADD = libuim-scm.la libuim.la
libuim_fileio_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_fileio_la_CPPFLAGS = -I$(top_srcdir)

uim_plugin_LTLIBRARIES += libuim-socket.la
libuim_socket_la_SOURCES = socket.c
libuim_socket_la_LIBADD = libuim-scm.la libuim.la
libuim_socket_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_socket_la_CPPFLAGS = -I$(top_srcdir)

uim_plugin_LTLIBRARIES += libuim-process.la
libuim_process_la_SOURCES = process.c
libuim_process_la_LIBADD = libuim-scm.la libuim.la
libuim_process_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_process_la_CPPFLAGS = -I$(top_srcdir)

if NOTIFY
libuim_la_SOURCES += uim-notify.c
endif

libuim_custom_la_SOURCES = uim-custom.c

if M17NLIB
  uim_plugin_LTLIBRARIES += libuim-m17nlib.la
  libuim_m17nlib_la_SOURCES = m17nlib.c
  libuim_m17nlib_la_LIBADD = @M17NLIB_LIBS@ libuim-scm.la libuim.la
  libuim_m17nlib_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
  libuim_m17nlib_la_CPPFLAGS = -I$(top_srcdir) @M17NLIB_CFLAGS@
endif

if SCIM
  uim_plugin_LTLIBRARIES += libuim-scim.la
  libuim_scim_la_SOURCES = scim.cpp
  libuim_scim_la_LIBADD = @SCIM_LIBS@ libuim-scm.la libuim.la
  libuim_scim_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
  libuim_scim_la_CXXFLAGS = -I$(top_srcdir) @SCIM_CFLAGS@
endif

if CANNA
  uim_plugin_LTLIBRARIES += libuim-canna.la
  libuim_canna_la_SOURCES = canna.c
  libuim_canna_la_LIBADD = libuim-scm.la libuim.la -lcanna
  libuim_canna_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
  libuim_canna_la_CPPFLAGS = -I$(top_srcdir)
endif


if WNN
uim_plugin_LTLIBRARIES += libuim-wnn.la
libuim_wnn_la_SOURCES = wnnlib.h wnnlib.c
libuim_wnn_la_LIBADD = libuim.la $(WNN_LIBS) -lwnn @WNN_LIBADD@
libuim_wnn_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_wnn_la_CPPFLAGS = -I$(top_srcdir) $(WNN_CPPFLAGS)
endif

if SJ3
uim_plugin_LTLIBRARIES += libuim-sj3.la
libuim_sj3_la_SOURCES = sj3.c
libuim_sj3_la_LIBADD = @SJ3_LIBS@ libuim.la
libuim_sj3_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_sj3_la_CPPFLAGS = -I$(top_srcdir) @SJ3_CFLAGS@
endif

if ANTHY
if ENABLE_ANTHY_STATIC
  libuim_la_SOURCES += anthy.c
else
  uim_plugin_LTLIBRARIES += libuim-anthy.la
  libuim_anthy_la_SOURCES = anthy.c
  libuim_anthy_la_LIBADD = @ANTHY_LIBS@ libuim-scm.la libuim.la
  libuim_anthy_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
  libuim_anthy_la_CPPFLAGS = -I$(top_srcdir)
endif
endif

if ANTHY_UTF8
if ENABLE_ANTHY_UTF8_STATIC
  libuim_la_SOURCES += anthy-utf8.c
else
  uim_plugin_LTLIBRARIES += libuim-anthy-utf8.la
  libuim_anthy_utf8_la_SOURCES = anthy-utf8.c
  libuim_anthy_utf8_la_LIBADD = @ANTHY_UTF8_LIBS@ libuim-scm.la libuim.la
  libuim_anthy_utf8_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
  libuim_anthy_utf8_la_CPPFLAGS = -I$(top_srcdir) @ANTHY_UTF8_CFLAGS@
endif
endif

if MANA
uim_plugin_LTLIBRARIES += libuim-mana.la
libuim_mana_la_SOURCES = mana.c
libuim_mana_la_LIBADD = libuim-scm.la libuim.la
libuim_mana_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_mana_la_CPPFLAGS = -I$(top_srcdir)
endif

if PRIME
  uim_plugin_LTLIBRARIES += libuim-prime.la
  libuim_prime_la_SOURCES = prime.c
  libuim_prime_la_LIBADD = libuim-scm.la libuim.la
  libuim_prime_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
  libuim_prime_la_CPPFLAGS = -I$(top_srcdir)
endif

if CURL
uim_plugin_LTLIBRARIES += libuim-curl.la
libuim_curl_la_SOURCES = curl.c
libuim_curl_la_LIBADD = @CURL_LIBS@ libuim.la
libuim_curl_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_curl_la_CPPFLAGS = -I$(top_srcdir) @CURL_CFLAGS@
endif

if EXPAT
uim_plugin_LTLIBRARIES += libuim-expat.la
libuim_expat_la_SOURCES = expat.c
libuim_expat_la_LIBADD = @EXPAT_LIBS@ libuim.la
libuim_expat_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_expat_la_CPPFLAGS = -I$(top_srcdir) @EXPAT_CFLAGS@
endif

uim_plugin_LTLIBRARIES += libuim-skk.la
libuim_skk_la_SOURCES = skk.c bsdlook.h
libuim_skk_la_LIBADD = libuim-scm.la libuim.la libuim-bsdlook.la @NETLIBS@
libuim_skk_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_skk_la_CPPFLAGS = -I$(top_srcdir)

uim_plugin_LTLIBRARIES += libuim-look.la
libuim_look_la_SOURCES = look.c bsdlook.h
libuim_look_la_LIBADD = libuim-scm.la libuim.la libuim-bsdlook.la
libuim_look_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_look_la_CPPFLAGS = -I$(top_srcdir)

libuim_bsdlook_la_SOURCES = bsdlook.h bsdlook.c
libuim_bsdlook_la_LIBADD =
libuim_bsdlook_la_CPPFLAGS = -I$(top_srcdir)

libuimincludedir =  $(includedir)/uim

libuim_scm_la_SOURCES = uim-scm-sigscheme.c
libuim_scm_la_LIBADD = $(GCROOTS_LIBS)
libuim_scm_la_LDFLAGS = -version-info $(libuim_scm_version) -export-dynamic
libuim_scm_la_CPPFLAGS = -I$(top_srcdir) \
			 -I$(top_builddir)/sigscheme/include \
			 -I$(top_srcdir)/sigscheme/src \
			 -I$(top_builddir)/sigscheme/src \
			 $(GCROOTS_CFLAGS)
if DEBUG
  libuim_scm_la_CPPFLAGS += -DDEBUG=1 -DDEBUG_SCM=1
endif
# Since the generated Makefile will have the order
# '$(libuim_scm_la_CFLAGS) $(CFLAGS)', overriding -Werror in $CFLAGS
# by -Wno-error in $UIM_SCM_CFLAGS does not work.
#libuim_scm_la_CFLAGS = $(UIM_SCM_CFLAGS)

# NOTE: Don't depend on -export-symbols-regex to avoid symbol
# conflict. The libtool option is not and will not be supported on
# some platforms. See [Anthy-dev 2847].  -- YamaKen 2006-03-30
libuim_la_LDFLAGS = -version-info $(libuim_version) -export-dynamic
libuim_la_LIBADD = @LTLIBINTL@ @LTLIBICONV@ \
		   libuim-scm.la \
		   $(top_builddir)/replace/libreplace.la
# - Place -I$(top_srcdir) surely prior to sigscheme dirs
# - To take precedence over installed gcroots.h by the bundled
#   libgcroots/include/gcroots.h, @GCROOTS_CFLAGS@ must be placed here.
libuim_la_CPPFLAGS = $(uim_defs) \
		     -I$(top_srcdir) \
		     -DPKGLIBDIR=\"$(pkglibdir)\" \
		     -DPKGDATADIR=\"$(pkgdatadir)\"

if ENABLE_ANTHY_STATIC
  libuim_la_LIBADD += @ANTHY_LIBS@
  libuim_la_CPPFLAGS += -DENABLE_ANTHY_STATIC
endif
if ENABLE_ANTHY_UTF8_STATIC
  libuim_la_LIBADD += @ANTHY_LIBS@
  libuim_la_CPPFLAGS += -DENABLE_ANTHY_UTF8_STATIC
endif
if DEBUG
  libuim_la_CPPFLAGS += -DDEBUG=1 -DDEBUG_SCM=1
endif

libuim_custom_la_LDFLAGS = -version-info $(libuim_custom_version) \
			   -export-dynamic
libuim_custom_la_LIBADD = libuim-scm.la libuim.la
libuim_custom_la_CPPFLAGS = $(uim_defs) -I$(top_srcdir)

uim_plugin_LTLIBRARIES += libuim-custom-enabler.la
libuim_custom_enabler_la_SOURCES = uim-custom-enabler.c
libuim_custom_enabler_la_LIBADD = libuim-custom.la libuim-scm.la libuim.la
libuim_custom_enabler_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
libuim_custom_enabler_la_CPPFLAGS = -I$(top_srcdir)

libuim_counted_init_la_SOURCES = counted-init.c counted-init.h
libuim_counted_init_la_CPPFLAGS = -I$(top_srcdir)

if LIBUIM_X_UTIL
libuim_x_util_la_SOURCES = uim-x-util.h uim-x-kana-input-hack.c
libuim_x_util_la_CPPFLAGS = -I$(top_srcdir)
libuim_x_util_la_CFLAGS = @X_CFLAGS@
endif


bin_PROGRAMS = uim-sh uim-module-manager
libexec_PROGRAMS = uim-helper-server

uim_helper_server_LIBS =  
uim_helper_server_CPPFLAGS = $(uim_defs) -I$(top_srcdir)
# uim-notify workaround for uim-error.c combined with non-libuim program
uim_helper_server_CPPFLAGS += -DUIM_NON_LIBUIM_PROG
uim_helper_server_CFLAGS =
uim_helper_server_SOURCES = uim-helper.c uim-helper-server.c uim-error.c
uim_helper_server_LDADD = $(top_builddir)/replace/libreplace.la

uim_sh_LIBS =
uim_sh_CPPFLAGS = $(uim_defs) -I$(top_srcdir)
uim_sh_CFLAGS =
uim_sh_LDADD = libuim-scm.la libuim.la
uim_sh_SOURCES = uim-sh.c

if LIBEDIT
  uim_plugin_LTLIBRARIES += libuim-editline.la
  libuim_editline_la_SOURCES = editline.c
  libuim_editline_la_LIBADD = @LIBEDIT_LIBS@ libuim-scm.la libuim.la
  libuim_editline_la_LDFLAGS = -rpath $(uim_plugindir) -avoid-version -module
  libuim_editline_la_CPPFLAGS = -I$(top_srcdir)
endif

uim_module_manager_LIBS =
uim_module_manager_CPPFLAGS = $(uim_defs) -I$(top_srcdir) -DUIM_DATADIR=\""$(datadir)/uim"\"
uim_module_manager_CFLAGS =
uim_module_manager_LDADD = libuim-scm.la libuim.la
uim_module_manager_SOURCES = uim-module-manager.c

noinst_PROGRAMS = uim-agent

uim_agent_SOURCES = agent.c
uim_agent_LDADD   = libuim-scm.la libuim.la
