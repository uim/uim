AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/src
AM_LDFLAGS  = -rpath $(libdir)
LIBS        = $(top_builddir)/src/libsscm.la

EXTRA_DIST= sscm-test.h utils.c \
            test-gc.c test-storage.c test-storage-compact.c \
            test-length.c test-strcasecmp.c test-format.c

SUFFIXES= -coll.o -coll.c

if MAINTAINER_MODE
CLEANFILES= *-coll.c

# FIXME: how do I make this rule depend on collect.sh (is it
# possible)?
.c-coll.c:
	$(SH) ./collect.sh $< > $@ || { rm -f $@; false; }
endif


# Name your test suite with a -coll suffix if you want it processed
# with collect.sh.  Leave the suffix out if you don't want some script
# messing around with your masterpiece.
TESTS= test-gc-coll test-storage-coll test-storage-compact-coll \
       test-length-coll test-strcasecmp-coll test-format-coll
check_PROGRAMS= $(TESTS) test-minishell

# GNU version of sed is needed to run collect.sh
dist-hook:
	@test -n "$(GSED)" || (echo 'GNU sed(1) not found: install it'; exit 1)
