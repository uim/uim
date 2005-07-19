lib_LTLIBRARIES  = libsscm.la
libsscm_la_SOURCES = \
		datas.c debug.c \
		encoding.c error.c \
		eval.c io.c \
		operations.c \
		read.c sigscheme.c  \
		sigscheme.h sigschemetype.h
 
libsscm_la_CFLAGS   = -Wall

bin_PROGRAMS  = sscm
sscm_SOURCES  = main.c
sscm_CFLAGS   = -Wall
sscm_LDADD    = $(top_builddir)/sigscheme/libsscm.la
