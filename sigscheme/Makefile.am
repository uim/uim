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

sigschemefunctable.c:./script/build_func_table.rb
	./script/build_func_table.rb > sigschemefunctable.c

libsscm_la_CFLAGS   = -Wall

bin_PROGRAMS  = sscm
sscm_SOURCES  = main.c
sscm_CFLAGS   = -Wall
sscm_LDADD    = $(top_builddir)/sigscheme/libsscm.la
