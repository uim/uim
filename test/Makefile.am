# Type 'make check' to run all tests.

uim_tests = \
        test-fail.scm \
        test-light-record.scm \
        test-template.scm \
        test-wlos.scm \
        util/test-list.scm \
        util/test-r5rs.scm \
        util/test-srfi.scm \
        util/test-string.scm
uim_optional_tests =
uim_xfail_tests = test-fail.scm

TESTS_ENVIRONMENT = \
	LIBUIM_SYSTEM_SCM_FILES="$(abs_top_srcdir)/subprojects/sigscheme/lib" \
	LIBUIM_SCM_FILES="$(abs_top_srcdir)/scm" \
	LIBUIM_PLUGIN_LIB_DIR="$(abs_top_builddir)/uim/.libs"
LOG_COMPILER = $(SH)
LOG_FLAGS = run.sh
if ENABLE_TESTS
TESTS = $(uim_tests) $(uim_optional_tests)
XFAIL_TESTS = $(uim_xfail_tests)
endif

EXTRA_DIST = run.sh.in $(uim_tests) meson.build
# The old tests for Gauche and GaUnit. They are being ported to the
# unittest framework of SigScheme.
EXTRA_DIST += uim-test-utils.scm run-test.scm template.scm \
        uim-test.scm uim-test-utils-new.scm uim-assertions.scm \
        test-action.scm test-custom-rt.scm test-custom.scm \
        test-im.scm test-intl.scm \
        test-lazy-load.scm test-plugin.scm \
        test-uim-test-utils.scm test-ustr.scm \
        test-example.scm \
        test-anthy.scm \
        i18n/test-base.scm \
        i18n/test-language.scm \
        key/test-base.scm \
        key/test-predicate.scm \
        key/test-translator.scm \
        util/test-character-conversion.scm \
        util/test-character-predicate.scm \
        util/test-misc.scm \
        util/test-multi-segment.scm \
        util/test-record.scm \
        util/test-rk.scm \
        util/test-uim.scm

DISTCLEANFILES = run.sh
