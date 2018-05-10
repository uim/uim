# Type 'make check' to run all tests.

uim_tests = \
        test-composer.scm \
        test-fail.scm \
        test-light-record.scm \
        test-template.scm \
        test-trec.scm \
        test-wlos.scm
uim_optional_tests =
uim_xfail_tests = test-fail.scm

TESTS_ENVIRONMENT = \
	LIBUIM_SYSTEM_SCM_FILES="$(abs_top_srcdir)/sigscheme/lib" \
	LIBUIM_SCM_FILES="$(abs_top_srcdir)/scm" \
	LIBUIM_PLUGIN_LIB_DIR="$(abs_top_builddir)/uim/.libs"
LOG_COMPILER = $(abs_top_builddir)/uim/uim-sh
TESTS = $(uim_tests) $(uim_optional_tests)
XFAIL_TESTS = $(uim_xfail_tests)

EXTRA_DIST = run-singletest.sh.in $(uim_tests)
DISTCLEANFILES = run-singletest.sh
