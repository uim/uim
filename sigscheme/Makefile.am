BUILD_FUNCTBL = ./script/build_func_table.rb

noinst_LTLIBRARIES  = libsscm.la

FUNC_TABLES = \
		sigschemefunctable-r5rs.c \
		sigschemefunctable-r5rs-deepcadrs.c \
		sigschemefunctable-error.c \
		sigschemefunctable-nonstd.c \
		sigschemefunctable-siod.c \
		sigschemefunctable-srfi1.c \
		sigschemefunctable-srfi2.c \
		sigschemefunctable-srfi6.c \
		sigschemefunctable-srfi8.c \
		sigschemefunctable-srfi23.c \
		sigschemefunctable-srfi34.c \
		sigschemefunctable-new-srfi34.c \
		sigschemefunctable-srfi38.c \
		sigschemefunctable-srfi60.c

# FIXME: simplify like sigschemefunctable-new-srfi34.c
sigschemefunctable.c: $(FUNC_TABLES)
sigschemefunctable-r5rs.c: ./script/build_func_table.rb sigscheme.c operations.c eval.c io.c
	./script/build_func_table.rb "" "r5rs_func_info_table" "sigscheme.c" "operations.c" "eval.c" "io.c" \
		> sigschemefunctable-r5rs.c
sigschemefunctable-r5rs-deepcadrs.c: ./script/build_func_table.rb operations-r5rs-deepcadrs.c
	./script/build_func_table.rb "" "r5rs_deepcadrs_func_info_table" "operations-r5rs-deepcadrs.c" \
		> sigschemefunctable-r5rs-deepcadrs.c
sigschemefunctable-error.c: error.c $(BUILD_FUNCTBL)
	$(BUILD_FUNCTBL) "" "scm_error_func_info_table" $< > $@
sigschemefunctable-nonstd.c: ./script/build_func_table.rb operations-nonstd.c
	./script/build_func_table.rb "" "nonstd_func_info_table" "operations-nonstd.c" \
		> sigschemefunctable-nonstd.c
sigschemefunctable-srfi1.c: ./script/build_func_table.rb operations-srfi1.c
	./script/build_func_table.rb "_SRFI1_" "srfi1_func_info_table" "operations-srfi1.c" \
		> sigschemefunctable-srfi1.c
sigschemefunctable-srfi2.c: ./script/build_func_table.rb operations-srfi2.c
	./script/build_func_table.rb "_SRFI2_" "srfi2_func_info_table" "operations-srfi2.c" \
		> sigschemefunctable-srfi2.c
sigschemefunctable-srfi6.c: ./script/build_func_table.rb operations-srfi6.c
	./script/build_func_table.rb "_SRFI6_" "srfi6_func_info_table" "operations-srfi6.c" \
		> sigschemefunctable-srfi6.c
sigschemefunctable-srfi8.c: ./script/build_func_table.rb operations-srfi8.c
	./script/build_func_table.rb "_SRFI8_" "srfi8_func_info_table" "operations-srfi8.c" \
		> sigschemefunctable-srfi8.c
sigschemefunctable-srfi23.c: ./script/build_func_table.rb operations-srfi23.c
	./script/build_func_table.rb "_SRFI23_" "srfi23_func_info_table" "operations-srfi23.c" \
		> sigschemefunctable-srfi23.c
sigschemefunctable-srfi34.c: ./script/build_func_table.rb operations-srfi34.c
	./script/build_func_table.rb "_SRFI34_" "srfi34_func_info_table" "operations-srfi34.c" \
		> sigschemefunctable-srfi34.c
sigschemefunctable-new-srfi34.c: operations-new-srfi34.c $(BUILD_FUNCTBL)
	$(BUILD_FUNCTBL) "_SRFI34_" "scm_new_srfi34_func_info_table" $< > $@
sigschemefunctable-srfi38.c: ./script/build_func_table.rb operations-srfi38.c
	./script/build_func_table.rb "_SRFI38_" "srfi38_func_info_table" "operations-srfi38.c" \
		> sigschemefunctable-srfi38.c
sigschemefunctable-srfi60.c: ./script/build_func_table.rb operations-srfi60.c
	./script/build_func_table.rb "_SRFI60_" "srfi60_func_info_table" "operations-srfi60.c" \
		> sigschemefunctable-srfi60.c
sigschemefunctable-siod.c: ./script/build_func_table.rb operations-siod.c
	./script/build_func_table.rb "" "siod_func_info_table" "operations-siod.c" \
		> sigschemefunctable-siod.c

# FIXME: add all distribution files to EXTRA_DIST
EXTRA_DIST = $(FUNC_TABLES)
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

libsscm_la_CFLAGS   = -Wall

bin_PROGRAMS  = sscm
sscm_SOURCES  = main.c
sscm_CFLAGS   = -Wall
sscm_LDADD    = $(top_builddir)/sigscheme/libsscm.la

CLEANFILES = \
	$(FUNC_TABLES)
