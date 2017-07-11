# Type 'make check' to run all tests.

uim_tests = \
        $(abs_top_srcdir)/test2/test-composer.scm \
        $(abs_top_srcdir)/test2/test-fail.scm \
        $(abs_top_srcdir)/test2/test-light-record.scm \
        $(abs_top_srcdir)/test2/test-template.scm \
        $(abs_top_srcdir)/test2/test-trec.scm \
        $(abs_top_srcdir)/test2/test-wlos.scm
uim_optional_tests =
uim_xfail_tests = $(abs_top_srcdir)/test2/test-fail.scm

TESTS_ENVIRONMENT = \
	LIBUIM_SYSTEM_SCM_FILES="$(abs_top_srcdir)/sigscheme/lib" \
	LIBUIM_SCM_FILES="$(abs_top_srcdir)/scm" \
	LIBUIM_PLUGIN_LIB_DIR="$(abs_top_builddir)/uim/.libs"
LOG_COMPILER = $(abs_top_builddir)/uim/uim-sh
TESTS = $(uim_tests) $(uim_optional_tests)
XFAIL_TESTS = $(uim_xfail_tests)

EXTRA_DIST = run-singletest.sh.in $(uim_tests)
DISTCLEANFILES = run-singletest.sh
