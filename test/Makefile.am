# Type 'make check' to run all tests.

uim_tests = \
        test-action.scm \
        test-anthy.scm \
        test-custom-rt.scm \
        test-custom.scm \
        test-fail.scm \
        test-hangul.scm \
        test-iconv.scm \
        test-im.scm \
        test-lazy-load.scm \
        test-light-record.scm \
        test-plugin.scm \
        test-skk-custom.scm \
        test-template.scm \
        test-ustr.scm \
        test-wlos.scm \
        i18n/test-base.scm \
        i18n/test-language.scm \
        key/test-base.scm \
        key/test-predicate.scm \
        key/test-translator.scm \
        util/test-character-conversion.scm \
        util/test-character-predicate.scm \
        util/test-list.scm \
        util/test-misc.scm \
        util/test-r5rs.scm \
        util/test-record.scm \
        util/test-rk.scm \
        util/test-srfi.scm \
        util/test-string.scm \
        util/test-uim.scm
uim_optional_tests =
uim_xfail_tests = test-fail.scm

TESTS_ENVIRONMENT = \
	LIBUIM_SYSTEM_SCM_FILES="$(abs_top_srcdir)/subprojects/sigscheme/lib" \
	LIBUIM_SCM_FILES="$(abs_top_srcdir)/scm" \
	LIBUIM_PLUGIN_LIB_DIR="$(abs_top_builddir)/uim/.libs" \
	LIBUIM_VANILLA=1
LOG_COMPILER = $(SH)
LOG_FLAGS = run.sh
if ENABLE_TESTS
TESTS = $(uim_tests) $(uim_optional_tests)
XFAIL_TESTS = $(uim_xfail_tests)
endif

EXTRA_DIST = run.sh.in $(uim_tests) meson.build \
	im/fake-dictionary.rb \
	im/im-driver.c \
	im/im-driver.rb \
	im/run.rb \
	im/test-canna.rb \
	im/test-google-cgiapi-jp.rb

DISTCLEANFILES = run.sh
