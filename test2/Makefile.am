# Type 'make check' to run all tests.

uim_tests = \
        test-fail.scm \
        test-light-record.scm \
        test-template.scm \
        test-wlos.scm
uim_optional_tests =
uim_xfail_tests = test-fail.scm

TESTS_ENVIRONMENT = \
	LIBUIM_SYSTEM_SCM_FILES="$(abs_top_srcdir)/subprojects/sigscheme/lib" \
	LIBUIM_SCM_FILES="$(abs_top_srcdir)/scm" \
	LIBUIM_PLUGIN_LIB_DIR="$(abs_top_builddir)/uim/.libs"
LOG_COMPILER = $(SH)
LOG_FLAGS = run-singletest.sh
if ENABLE_TESTS
TESTS = $(uim_tests) $(uim_optional_tests)
XFAIL_TESTS = $(uim_xfail_tests)
endif

EXTRA_DIST = run-singletest.sh.in $(uim_tests) meson.build
DISTCLEANFILES = run-singletest.sh
