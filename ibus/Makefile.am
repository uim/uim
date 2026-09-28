EXTRA_DIST =				\
	test/candidates.scm		\
	test/helper.rb			\
	test/run.rb			\
	test/test-ibus-engine-uim.rb	\
	uim.xml.in

if IBUS

AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

libexec_PROGRAMS = ibus-engine-uim

ibus_engine_uim_SOURCES = ibus-engine-uim.c
ibus_engine_uim_CPPFLAGS = $(AM_CPPFLAGS)
ibus_engine_uim_CFLAGS = @IBUS_CFLAGS@ -Wall
ibus_engine_uim_LDADD = $(top_builddir)/uim/libuim-scm.la \
			$(top_builddir)/uim/libuim.la \
			@IBUS_LIBS@

# ibus-daemon lists the engines described in this directory.
componentdir = $(datadir)/ibus/component
component_DATA = uim.xml
CLEANFILES = uim.xml

uim.xml: uim.xml.in
	sed -e 's|@libexecdir[@]|$(libexecdir)|g' \
	    -e 's|@PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g' \
	    -e 's|@uim_pixmapsdir[@]|$(uim_pixmapsdir)|g' $< > $@

# An ibus-daemon of the test's own, driven through libibus as GNOME
# Shell drives it. It needs no display and no desktop, but Ruby/GObject
# Introspection and the IBus typelib.
if IBUS_TEST

TESTS_ENVIRONMENT = \
	IBUS_ENGINE_UIM="$(abs_builddir)/ibus-engine-uim" \
	IBUS_DAEMON="$(IBUS_DAEMON)" \
	LIBUIM_SYSTEM_SCM_FILES="$(abs_top_srcdir)/sigscheme/lib" \
	LIBUIM_SCM_FILES="$(abs_top_builddir)/scm:$(abs_top_srcdir)/scm" \
	LIBUIM_PLUGIN_LIB_DIR="$(abs_top_builddir)/uim/.libs"
TEST_EXTENSIONS = .rb
RB_LOG_COMPILER = $(DBUS_RUN_SESSION) -- $(RUBY)
TESTS = test/run.rb

endif

endif
