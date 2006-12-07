AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/src
AM_LDFLAGS  = -rpath $(libdir)
LIBS        = $(top_builddir)/src/libsscm.la

EXTRA_DIST = \
        test_template.c test-storage-compact.c \
        README cutter-r20-yamaken-extension.diff
noinst_LTLIBRARIES =

if HAVE_CUTTER_H
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
