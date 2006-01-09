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
		sigschemefunctable-srfi38.c \
		sigschemefunctable-srfi60.c

BUILD_FUNCTBL = ./script/build_func_table.rb
BUILD_FUNCTBL_SOURCES = \
		$(BUILD_FUNCTBL) \
		./script/scm_decl.rb
		./script/functable-header.txt \
		./script/functable-footer.txt

sigschemefunctable.c: $(FUNC_TABLES)
sigschemefunctable-r5rs.c: sigscheme.c operations.c eval.c syntax.c io.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "r5rs_func_info_table" \
	  sigscheme.c operations.c eval.c syntax.c io.c > $@
sigschemefunctable-r5rs-deepcadrs.c: operations-r5rs-deepcadrs.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "r5rs_deepcadrs_func_info_table" $< > $@
sigschemefunctable-error.c: error.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_error_func_info_table" $< > $@
sigschemefunctable-nonstd.c: operations-nonstd.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "nonstd_func_info_table" $< > $@
sigschemefunctable-srfi1.c: operations-srfi1.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "srfi1_func_info_table" $< > $@
sigschemefunctable-srfi2.c: operations-srfi2.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "srfi2_func_info_table" $< > $@
sigschemefunctable-srfi6.c: operations-srfi6.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "srfi6_func_info_table" $< > $@
sigschemefunctable-srfi8.c: operations-srfi8.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "srfi8_func_info_table" $< > $@
sigschemefunctable-srfi23.c: operations-srfi23.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "srfi23_func_info_table" $< > $@
sigschemefunctable-srfi34.c: operations-srfi34.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_new_srfi34_func_info_table" $< > $@
sigschemefunctable-srfi38.c: operations-srfi38.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "srfi38_func_info_table" $< > $@
sigschemefunctable-srfi60.c: operations-srfi60.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "srfi60_func_info_table" $< > $@
sigschemefunctable-siod.c: operations-siod.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "siod_func_info_table" $< > $@

EXTRA_DIST = $(FUNC_TABLES) $(BUILD_FUNCTBL_SOURCES) \
        storage-fatty.h storage-compact.h
libsscm_la_SOURCES = \
		alloc.c storage.c write.c \
                storage-gc.c \
                storage-symbol.c \
		storage-continuation.c \
		encoding.c error.c \
		env.c eval.c syntax.c io.c \
                basecport.c fileport.c \
		operations.c \
		read.c sigscheme.c sigschemefunctable.c \
		sigscheme.h sigschemefunctable.h

libsscm_la_CFLAGS   = -Wall

bin_PROGRAMS  = sscm
sscm_SOURCES  = main.c
sscm_CFLAGS   = -Wall
sscm_LDADD    = $(top_builddir)/sigscheme/libsscm.la

CLEANFILES = \
	$(FUNC_TABLES)
