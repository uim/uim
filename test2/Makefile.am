uim_tests = 
uim_optional_tests = test-template.scm
uim_xfail_tests =

TESTS_ENVIRONMENT = $(SH) $(top_builddir)/test2/run-singletest.sh
TESTS = $(uim_tests) $(uim_optional_tests)
XFAIL_TESTS = $(uim_xfail_tests)

EXTRA_DIST = run-singletest.sh.in $(uim_tests)
DISTCLEANFILES = run-singletest.sh
