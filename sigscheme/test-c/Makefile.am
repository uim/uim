AM_CPPFLAGS = -I$(top_srcdir)/src
AM_LDFLAGS  = -rpath $(libdir)
LIBS        = $(top_builddir)/src/libsscm.la

EXTRA_DIST = \
        test_template.c test-storage-compact.c \
        README cutter-r20-yamaken-extension.diff
noinst_LTLIBRARIES =

if HAVE_CUTTER_H
noinst_LTLIBRARIES            += libtest_strcasecmp.la
libtest_strcasecmp_la_SOURCES = test_strcasecmp.c cutter-sscm.h

noinst_LTLIBRARIES         += libtest_global.la
libtest_global_la_SOURCES  = test_global.c cutter-sscm.h

noinst_LTLIBRARIES         += libtest_format.la
libtest_format_la_SOURCES  = test_format.c cutter-sscm.h
endif

.PHONY: test update-suites

test:
	$(CUTTER) $(objdir)

update-suites:
	@for src in $(DIST_SOURCES); do         \
	    case $$src in                       \
	    test_*.c)                           \
	        cutter-update-suite $$src;      \
		;;                              \
	    esac                                \
	done
