EXTRA_DIST =				\
	test/run.rb			\
	test/test-uim-wayland.rb	\
	uim-wayland.desktop.in

if WAYLAND

AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)				\
	-I$(builddir)

bin_PROGRAMS = uim-wayland

uim_wayland_SOURCES = \
	uim-wayland.c uim-wayland.h \
	key.c \
	candwin.c \
	helper.c
nodist_uim_wayland_SOURCES = $(PROTOCOL_SOURCES)
uim_wayland_CPPFLAGS = $(AM_CPPFLAGS)
uim_wayland_CFLAGS = @WAYLAND_CFLAGS@ -Wall
uim_wayland_LDADD = $(top_builddir)/uim/libuim-scm.la \
		    $(top_builddir)/uim/libuim.la \
		    @WAYLAND_LIBS@

# Client code for zwp_input_method_v1 generated from wayland-protocols.
PROTOCOL_SOURCES = \
	input-method-unstable-v1-client-protocol.h \
	input-method-unstable-v1-protocol.c
BUILT_SOURCES = $(PROTOCOL_SOURCES)
CLEANFILES = $(PROTOCOL_SOURCES) uim-wayland.desktop

INPUT_METHOD_XML = \
	$(WAYLAND_PROTOCOLS_DIR)/unstable/input-method/input-method-unstable-v1.xml

input-method-unstable-v1-client-protocol.h: $(INPUT_METHOD_XML)
	$(WAYLAND_SCANNER) client-header $< $@

input-method-unstable-v1-protocol.c: $(INPUT_METHOD_XML)
	$(WAYLAND_SCANNER) private-code $< $@

# KWin lists input methods from desktop files that have
# X-KDE-Wayland-VirtualKeyboard=true.
desktopdir = $(datadir)/applications
desktop_DATA = uim-wayland.desktop

uim-wayland.desktop: uim-wayland.desktop.in
	sed -e 's|@bindir[@]|$(bindir)|g' \
	    -e 's|@uim_pixmapsdir[@]|$(uim_pixmapsdir)|g' $< > $@

# A compositor that drives uim-wayland over a socketpair. It needs no
# display and no compositor, so it can run unattended. It reads the
# wire format out of the same protocol descriptions wayland-scanner
# uses, so it needs no library of its own.
if WAYLAND_TEST

TESTS_ENVIRONMENT = \
	UIM_WAYLAND="$(abs_builddir)/uim-wayland" \
	WAYLAND_XML="$(WAYLAND_CORE_XML)" \
	INPUT_METHOD_XML="$(INPUT_METHOD_XML)" \
	LIBUIM_SYSTEM_SCM_FILES="$(abs_top_srcdir)/sigscheme/lib" \
	LIBUIM_SCM_FILES="$(abs_top_builddir)/scm:$(abs_top_srcdir)/scm" \
	LIBUIM_PLUGIN_LIB_DIR="$(abs_top_builddir)/uim/.libs"
TEST_EXTENSIONS = .rb
RB_LOG_COMPILER = $(RUBY)
TESTS = test/run.rb

endif

endif
