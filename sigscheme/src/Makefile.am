#noinst_LTLIBRARIES = libsscm.la
lib_LTLIBRARIES = libsscm.la

FUNC_TABLES = \
		sigschemefunctable-r5rs-syntax.c \
		sigschemefunctable-r5rs-procedure.c \
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

# FIXME: separate non-core R5RS features
R5RS_PROC_SRCS = sigscheme.c procedure.c eval.c list.c number.c string.c \
                 vector.c port.c read.c write.c load.c

.PHONY: func-tables
func-tables: $(FUNC_TABLES)
sigschemefunctable-r5rs-syntax.c: syntax.c module.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_r5rs_syntax_func_info_table" \
          syntax.c module.c > $@
sigschemefunctable-r5rs-procedure.c: $(R5RS_PROC_SRCS) $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_r5rs_procedure_func_info_table" \
	  $(R5RS_PROC_SRCS) > $@
sigschemefunctable-r5rs-deepcadrs.c: module-r5rs-deepcadrs.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_r5rs_deepcadrs_func_info_table" $< > $@
sigschemefunctable-error.c: error.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_error_func_info_table" $< > $@
sigschemefunctable-nonstd.c: module-nonstd.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_nonstd_func_info_table" $< > $@
sigschemefunctable-srfi1.c: module-srfi1.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_srfi1_func_info_table" $< > $@
sigschemefunctable-srfi2.c: module-srfi2.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_srfi2_func_info_table" $< > $@
sigschemefunctable-srfi6.c: module-srfi6.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_srfi6_func_info_table" $< > $@
sigschemefunctable-srfi8.c: module-srfi8.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_srfi8_func_info_table" $< > $@
sigschemefunctable-srfi23.c: module-srfi23.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_srfi23_func_info_table" $< > $@
sigschemefunctable-srfi34.c: module-srfi34.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_srfi34_func_info_table" $< > $@
sigschemefunctable-srfi38.c: module-srfi38.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_srfi38_func_info_table" $< > $@
sigschemefunctable-srfi60.c: module-srfi60.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_srfi60_func_info_table" $< > $@
sigschemefunctable-siod.c: module-siod.c $(BUILD_FUNCTBL_SOURCES)
	$(BUILD_FUNCTBL) "scm_siod_func_info_table" $< > $@

EXTRA_DIST = $(FUNC_TABLES) $(BUILD_FUNCTBL_SOURCES) \
        test-compact.c

# Only works on 'make all', 'make check' and 'make install'.
# See "Built sources" section of automake info.
BUILT_SOURCES = $(FUNC_TABLES)

pkginclude_HEADERS = sigscheme.h config.h my-stdint.h

libsscm_la_SOURCES = \
        config-nonstd-string.h config-asprintf.h \
        alloc.c storage.c storage-gc.c storage-symbol.c \
        error.c env.c eval.c syntax.c procedure.c list.c \
        module.c \
        sigschemeinternal.h sigscheme.c
if USE_STORAGE_COMPACT
  pkginclude_HEADERS += storage-compact.h
else
  pkginclude_HEADERS += storage-fatty.h
endif
if USE_CONTINUATION
  libsscm_la_SOURCES += storage-continuation.c
endif
if USE_NUMBER
  libsscm_la_SOURCES += number.c
endif
if USE_STRING
  libsscm_la_SOURCES += string.c
endif
if USE_VECTOR
  libsscm_la_SOURCES += vector.c
endif
if USE_PORT
  libsscm_la_SOURCES += port.c baseport.h basecport.c fileport.h fileport.c
if USE_MULTIBYTE_CHAR
    libsscm_la_SOURCES += mbcport.h mbcport.c
else
    libsscm_la_SOURCES += sbcport.h sbcport.c
endif
endif
if USE_READER
  libsscm_la_SOURCES += read.c
endif
if USE_WRITER
  libsscm_la_SOURCES += write.c
endif
if USE_LOAD
  libsscm_la_SOURCES += load.c
endif
if USE_MULTIBYTE_CHAR
  pkginclude_HEADERS += encoding.h
  libsscm_la_SOURCES += encoding.c
endif
if USE_DEEP_CADRS
  libsscm_la_SOURCES += module-r5rs-deepcadrs.c
endif
if USE_NONSTD_FEATURES
  libsscm_la_SOURCES += module-nonstd.c
endif
if COMPAT_SIOD
  libsscm_la_SOURCES += module-siod.c nullport.h nullport.c
endif
if USE_SRFI1
  libsscm_la_SOURCES += module-srfi1.c
endif
if USE_SRFI2
  libsscm_la_SOURCES += module-srfi2.c
endif
if USE_SRFI6
  libsscm_la_SOURCES += module-srfi6.c strport.h strport.c
endif
if USE_SRFI8
  libsscm_la_SOURCES += module-srfi8.c
endif
if USE_SRFI23
  libsscm_la_SOURCES += module-srfi23.c
endif
if USE_SRFI34
  libsscm_la_SOURCES += module-srfi34.c
endif
if USE_SRFI38
  libsscm_la_SOURCES += module-srfi38.c
endif
if USE_SRFI60
  libsscm_la_SOURCES += module-srfi60.c
endif

libsscm_la_CFLAGS   = -Wall

bin_PROGRAMS  = sscm
sscm_SOURCES  = main.c
sscm_CFLAGS   = -Wall
sscm_LDADD    = $(top_builddir)/src/libsscm.la

CLEANFILES = \
	$(FUNC_TABLES)
