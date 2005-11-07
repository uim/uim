noinst_LTLIBRARIES  = libsscm.la
libsscm_la_SOURCES = \
		storage.c debug.c \
                storage-gc.c \
                storage-symbol.c \
		storage-continuation.c \
		encoding.c error.c \
		eval.c io.c \
		operations.c \
		read.c sigscheme.c sigschemefunctable.c \
		sigscheme.h sigschemetype.h sigschemefunctable.h

EXTRA_DIST = \
		operations-r5rs-deepcadrs.c \
		operations-nonstd.c operations-siod.c    \
		operations-srfi1.c operations-srfi2.c    \
		operations-srfi6.c operations-srfi8.c    \
		operations-srfi23.c operations-srfi34.c  \
		operations-srfi38.c operations-srfi60.c

sigschemefunctable.c: ./script/build_func_table.rb operations.c eval.c io.c $(EXTRA_DIST)
	./script/build_func_table.rb > sigschemefunctable.c

libsscm_la_CFLAGS   = -Wall

bin_PROGRAMS  = sscm
sscm_SOURCES  = main.c
sscm_CFLAGS   = -Wall
sscm_LDADD    = $(top_builddir)/sigscheme/libsscm.la
